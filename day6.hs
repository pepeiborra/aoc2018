#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package monoidal-containers --package hashable
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Category      ((>>>))
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable         (fold)
import           Data.Function
import           Data.Hashable
import           Data.HashMap.Monoidal (MonoidalHashMap, fromList, lookup,
                                        toList)
import           Data.List             (maximumBy, sort, sortBy, stripPrefix, sum)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Semigroup as S
import           GHC.Generics
import           Prelude               hiding (lookup)


main :: IO ()
main = interact script2

script, script2 :: String -> String
script input = unlines [rendered, show ("maxCol", maxCol grid), show ("maxRow", maxRow grid),  show (length $ snd $ solve grid)]
  where
    grid = (lines >>> map read >>> computeGrid) input
    rendered = renderWinner grid

script2 = lines >>> map read >>> computeSafeGrid 10000 >>> length >>> show

computeGrid :: [Coord] -> LocationGrid
computeGrid locations = Grid{..}
  where
    grid =
      fromList $
      [ (ix, [F C{..}])
      | col <- [1 .. maxCol]
      , row <- [1 .. maxRow]
      , Just ix <- [closest locations C{..}]
      ] ++
      [ (ix, [Inf])
      | col <- [0, maxCol+1] , row <- [0..maxRow+1]
      , Just ix <- [closest locations C{..}]
      ] ++
      [ (ix, [Inf])
      | col <- [0..maxCol+1] , row <- [0, maxRow+1]
      , Just ix <- [closest locations C{..}]
      ]
    maxCol = maximum (map col locations)
    maxRow = maximum (map row locations)

manhattanDistance x y = (abs $ col y - col x) + (abs $ row y - row x)

closest locations x =
      case sortBy (compare `on` snd) (zip [0 ..] (map (manhattanDistance x) locations)) of
           (ix, d):(_, d'):_
             | d /= d' -> Just ix
           _ -> Nothing

computeSafeGrid :: Int -> [Coord] -> [Coord]
computeSafeGrid maxDist locations =
      [ C{..}
      | col <- [1 .. maxCol]
      , row <- [1 .. maxRow]
      , sum (manhattanDistance C{..} <$> locations) < maxDist
      ]
  where
    maxCol = maximum (map col locations)
    maxRow = maximum (map row locations)

solve :: LocationGrid -> (LocationId, [Coord])
solve =
  maximumBy (compare `on` length . snd) .
  mapMaybe (sequence . second (finite . sequenceA)) . toList . grid

type LocationId = Int

type LocationGrid = Grid LocationId [Finite Coord]
type CoordGrid    = Grid Coord (S.First LocationId)

data Grid k v = Grid
  { maxCol, maxRow :: !Int
  , grid           :: MonoidalHashMap k v
  , locations      :: [Coord]
  }

renderWinner :: LocationGrid -> String
renderWinner g = renderGrid (\_ l -> if l == winner then 'O' else '.') (toCoordGrid g)
  where
    (winner, _) = solve g

renderAll :: LocationGrid -> String
renderAll g = renderGrid renderCell (toCoordGrid g)
  where
    (winner, _) = solve g
    renderCell _ c
      | c < ord 'z' - ord 'a' = chr $ ord 'a' + c
      | otherwise = chr $ ord 'A' + c

renderGrid :: (Coord -> LocationId -> Char) -> CoordGrid -> String
renderGrid renderCell g@Grid {..} =
  unlines
    [ [ maybe '.' (renderCell c . S.getFirst) (lookup c grid)
    | col <- [0 .. maxCol]
    , let c = C{..}
    ]
    | row <- [0 .. maxRow]
    ]

renderCell Grid{..} c l =
      (if c `elem` locations then toUpper else id) $
      (chr . (ord 'a' +)) l

toCoordGrid :: LocationGrid -> CoordGrid
toCoordGrid Grid{grid=lg, ..} = Grid{..} where
  grid = fromList [ (c, S.First l) | (l, cc) <- toList lg, F c <- cc]

data Coord = C { col, row :: !Int}
  deriving (Eq, Ord, Generic, Hashable)

instance Read Coord where
  readsPrec _ inp =
    [ (C{..}, inp'')
    | (col, ',':' ':inp') <- reads inp
    , (row, inp'') <- reads inp'
    ]

instance Show Coord where
  show C{..} = show col ++ ", " ++ show row

data Finite a = F !a | Inf
  deriving (Eq, Ord, Functor, Foldable, Traversable)

finite Inf   = Nothing
finite (F x) = Just x

instance Show a => Show (Finite a) where
  show (F x) = show x
  show Inf   = "_"

instance Semigroup a => Semigroup (Finite a) where
  F a <> F b = F (a <> b)
  _ <> _ = Inf

instance Monoid a => Monoid (Finite a) where
  mempty = F mempty

instance Applicative Finite where
  pure = F
  F f <*> F x = F (f x)
  _   <*> _   = Inf

example =
  [
  "1, 1",
  "1, 6",
  "8, 3",
  "3, 4",
  "5, 5",
  "8, 9"
  ]

exampleGrid = computeGrid $ map read example
