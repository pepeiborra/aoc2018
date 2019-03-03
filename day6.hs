#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package monoidal-containers
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

import           Control.Category      ((>>>))
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable         (fold)
import           Data.Function
import           Data.HashMap.Monoidal (MonoidalHashMap, fromList, toList)
import           Data.List             (maximumBy, sort, sortBy, stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Prelude               hiding (lookup)


main :: IO ()
main = interact script

script, script2 :: String -> String
script = lines >>> map read >>> computeGrid >>> solve >>> show

script2 = id

computeGrid :: [Coord] -> Grid
computeGrid coords =
  fromList $
  [ (ix, F 1)
  | col <- [1 .. mapCol]
  , row <- [1 .. maxRow]
  , Just ix <- [closest col row]
  ] ++
  [ (ix, Inf)
  | col <- [0, mapCol]
  , row <- [0, maxRow]
  , Just ix <- [closest col row]
  ]
  where
    mapCol = maximum (map col coords)
    maxRow = maximum (map row coords)
    closest col row =
      let manhattanDistance (_, C c r) = (abs $ col - c) + (abs $ row - r)
      in case sortBy (compare `on` manhattanDistance) (zip [0 ..] coords) of
           (ix, d):(_, d'):_
             | d /= d' -> Just ix
           _ -> Nothing

solve :: Grid -> Int
solve = snd . maximumBy (compare `on` snd) . mapMaybe (sequence . second finite) . toList

type Id = Int
type Grid = MonoidalHashMap Id FiniteInt

data Coord = C { col, row :: !Int}
  deriving (Eq, Ord)

instance Read Coord where
  readsPrec _ inp =
    [ (C x y, inp'')
    | (x, ',':' ':inp') <- reads inp
    , (y, inp'') <- reads inp'
    ]

data FiniteInt = F !Int | Inf
  deriving (Eq, Ord)

finite Inf   = Nothing
finite (F x) = Just x

instance Show FiniteInt where
  show (F x) = show x
  show Inf   = "_"

instance Semigroup FiniteInt where
  F a <> F b = F (a + b)
  _ <> _ = Inf

instance Monoid FiniteInt where
  mempty = F 0

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
