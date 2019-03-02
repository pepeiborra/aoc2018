#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package monoidal-containers
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

import Control.Category ((>>>))
import Data.List
import Data.Map.Monoidal.Strict (MonoidalMap, (!),  fromList, toList)
import Data.Maybe
import Data.Monoid
import Text.Printf

main :: IO ()
main = interact script2

script :: String -> String
script =
  lines >>>
  foldMap (read >>> claimToCells >>> map (,Sum 1) >>> fromList) >>>
  toList >>> countBy ((> 1) . getSum . snd) >>> show

script2 :: String -> String
script2 inp =
  show $
  maybe undefined index $
  find (claimToCells >>> all (\c -> cellMap ! c == 1)) claims
  where
    claims = map read $ lines inp
    cellMap = foldMap (claimToCells >>> map (, Sum 1) >>> fromList) claims

data Claim = Claim
  { index :: !Int
  , left, top :: !Int
  , width, height :: !Int
  }

instance Show Claim where
  show Claim{..} = printf "#%d @ %d,%d: %dx%d" index left top width height

-- #1 @ 1,3: 4x4
instance Read Claim where
  readsPrec _ ('#':x) =
    [ (Claim i l t w h, x''''')
    | (i, ' ':'@':' ':x') <- reads x
    , (l, ',':x'') <- reads x'
    , (t, ':':' ':x''') <- reads x''
    , (w, 'x':x'''') <- reads x'''
    , (h, x''''') <- reads x''''
    ]

data Cell = Cell { col, row :: !Int}
  deriving (Eq, Ord)

instance Show Cell where show Cell{..} = show (row, col)

claimToCells :: Claim -> [Cell]
claimToCells Claim {..} =
  [Cell{..} | col <- [left .. left + width - 1], row <- [top .. top + height - 1]]

countBy :: (a -> Bool) -> [a] -> Int
countBy pred aa = length (filter pred aa)


example =
  [ "#1 @ 1,3: 4x4"
  , "#2 @ 3,1: 4x4"
  , "#3 @ 5,5: 2x2"
  ]
exampleClaims :: [Claim]
exampleClaims = map read example

exampleCellMap = foldMap (claimToCells >>> map (, Sum 1) >>> fromList) exampleClaims

exampleWinner = find (claimToCells >>> all (\c -> exampleCellMap ! c == 1)) exampleClaims
