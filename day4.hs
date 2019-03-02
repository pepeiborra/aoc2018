#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package monoidal-containers --package time
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

import           Control.Category      ((>>>))
import           Data.Bifunctor
import           Data.Foldable         (fold)
import           Data.Function
import           Data.HashMap.Monoidal (MonoidalHashMap, elems, fromList,
                                        lookup, toList)
import           Data.List             (maximumBy, sort, stripPrefix)
import           Data.Monoid
import           Data.Time
import           Data.Time.Format
import           Prelude               hiding (lookup)

main :: IO ()
main = interact script2

script :: String -> String
script = lines >>> map read >>> sort >>> process >>> solve >>> (uncurry (*)) >>> show

script2 :: String -> String
script2 = lines >>> map read >>> sort >>> process >>> solve2 >>> (uncurry (*)) >>> show

process :: [Entry] -> GuardTable
process (Entry _ (BeginShift g):rest) = mconcat $ beginShift g rest
  where
    beginShift g []                             = []
    beginShift g (Entry _ (BeginShift g'):rest) = beginShift g' rest
    beginShift g (Entry t FallsAsleep:rest)     = fallsAsleep g t rest
    fallsAsleep g t (Entry t' WakesUp:rest) =
      fromList [(g, fromList [(m, 1) | m <- [minute t .. minute t' - 1]])] :
      beginShift g rest

solve :: GuardTable -> (GuardId, Minute)
solve gt = (g, m) where
  (g, mt) = maximumBy (compare `on` totalAsleep . snd) (toList gt)
  (m, _) = maximumBy (compare `on` snd) (toList mt)

solve2 :: GuardTable -> (GuardId, Minute)
solve2 = second fst . maximumBy (compare `on` snd . snd) . map (second maxMinute) . toList

minute :: UTCTime -> Minute
minute = (`div` 60) . floor . utctDayTime

type GuardId = Int
type Minute  = Int
type GuardTable = MonoidalHashMap GuardId MinuteTable
type MinuteTable = MonoidalHashMap Minute (Sum Int)

totalAsleep :: MinuteTable -> Int
totalAsleep = getSum . fold

maxMinute :: MinuteTable -> (Minute, Int)
maxMinute = second getSum . maximumBy (compare `on` snd) . toList

data Entry = Entry !UTCTime !Action
  deriving (Eq, Ord, Show)

data Action
  = BeginShift !Int
  | FallsAsleep
  | WakesUp
  | Other String
  deriving (Eq, Ord, Show)

instance Read Entry where
  readsPrec _ x =
    [ (Entry t a, x'')
    | (t, ' ':x') <- readSTime False defaultTimeLocale "[%F %R]" x
    , (a, x'') <- reads x'
    ]

instance Read Action where
  readsPrec _ x
    | Just rest <- stripPrefix "falls asleep" x = [(FallsAsleep, rest)]
  readsPrec _ x
    | Just rest <- stripPrefix "wakes up" x = [(WakesUp, rest)]
  readsPrec _ x =
    case
      [ (BeginShift n, rest)
      | Just x' <- [stripPrefix "Guard #" x]
      , (n, x'') <- reads x'
      , Just rest <- [stripPrefix " begins shift" x'']
      ] of
      [] -> [(Other x, [])]
      x  -> x

renderGuardTable :: GuardTable -> String
renderGuardTable table = unlines $ map renderGuardLine (toList table)
  where
    renderGuardLine (g, minutes) =
      '#' :
      show g ++
      " T" ++
      show (totalAsleep minutes) ++
      " " ++
      concat [maybe "." (show . getSum . min 9) (lookup i minutes) | i <- [0 .. 59]]

example :: [Entry]
example = map read
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  , "[1518-11-02 00:40] falls asleep"
  , "[1518-11-02 00:50] wakes up"
  , "[1518-11-03 00:05] Guard #10 begins shift"
  , "[1518-11-03 00:24] falls asleep"
  , "[1518-11-03 00:29] wakes up"
  , "[1518-11-04 00:02] Guard #99 begins shift"
  , "[1518-11-04 00:36] falls asleep"
  , "[1518-11-04 00:46] wakes up"
  , "[1518-11-05 00:03] Guard #99 begins shift"
  , "[1518-11-05 00:45] falls asleep"
  , "[1518-11-05 00:55] wakes up"
  ]

exampleTable = process example
