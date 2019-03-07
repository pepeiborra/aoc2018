#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --optimize --package mtl --package containers --package monoidal-containers --package hashable --package generic-lens --package lens --package scanf --package vector --package scanf --package ansi-terminal
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

import           Control.Arrow
import           Control.Category    ((>>>))
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable       (find, fold)
import qualified Data.Foldable       as F
import           Data.Function
import           Data.Graph
import           Data.Hashable
import           Data.Maybe
import           Data.Semigroup
import           Data.Set            as Set
import           GHC.Exts            (fromList)
import           Prelude             hiding (lookup)
import           System.Console.ANSI
import           System.Environment
import           System.IO
import           Text.Read           (readMaybe)
import           Text.Scanf


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  [fp] <- getArgs
  inp  <- readFile fp
  let initial = render $ fmap parsePoint $ lines inp
      initial' = stepBoard (+) initial
      jump = min ( (nrows initial' - 20) `div` (abs $ nrows initial' - nrows initial) )
                 ( (ncols initial' - 20) `div` (abs $ ncols initial' - ncols initial) )
      fastforward = stepBoard (\x v -> x + v * jump) initial'
  print ("ff", jump + 1)
  let loop board= do
        clearFromCursorToScreenEnd
        putStr (unlines $ rows board )
        c <- getChar
        cursorUpLine (length $ rows board)
        case c of
          'q'                         -> return ()
          '-'                         -> loop (stepBoard (-) board)
          n | Just n <- readMaybe [n] -> loop (stepBoard (\x v -> x + v*10^n) board)
          _                           -> loop (stepBoard (+) board)
  loop fastforward

data Point = P
  { x, y, vx, vy :: !Int
  } deriving (Show)

instance Eq Point where
  a == b = x a == x b && y a == y b

instance Ord Point where
  compare a b = compare (x a, y a) (x b, y b)

step :: (Int -> Int -> Int) -> Point -> Point
step f p@P{..} = p { x = f x vx, y = f y vy }

stepBoard f b = render $ fmap (step f) (points b)

parsePoint x =
  case scanf [fmt|position=<%d, %d> velocity=<%d,  %d>|] x of
    Just (x :+ y :+ vx :+ vy :+ ()) -> P{..}

data Board = B
  { rows         :: [String]
  , ncols, nrows :: !Int
  , points       :: [Point]
  }

render :: [Point] -> Board
render points = B{..}
  where
    rows
      | ncols <= 200 && nrows <= 200
      = show (nrows,ncols)
      : [ [ if P col row 0 0 `member` pointsSet then '#' else '.'
          | col <- [left .. right]]
        | row <- [top .. bot]]
      | otherwise
      = [show (nrows, ncols)]
    (Min top, Max bot, Min left, Max right) =
      foldMap (\P{..} -> (Min y, Max y, Min x, Max x)) points
    ncols = right - left
    nrows = bot - top
    pointsSet = Set.fromList points

example = "position=< 21518, -21209> velocity=<-2,  2>"
