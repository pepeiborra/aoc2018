#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package monoidal-containers
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

import Control.Category ((>>>))
import Data.Function
import Data.List
import Data.Map.Monoidal.Strict (MonoidalMap, elems)
import Data.Monoid

main :: IO ()
main = interact script2

script1, script2 :: String -> String
script1 = lines >>> map readCharTable >>> checksum >>> show
script2 = lines >>> matches >>> head

matches :: Ord a => [[a]] -> [[a]]
matches words = concat
  [ (concatMap (drop 1). group . sort . map (dropAt i)) words
  | i <- [0 .. length (head words) - 1]
  ]

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x:y:zz) = (x,y) : pairs (y:zz)

-- assumes equal length
almostEqual :: Eq a => [a] -> [a] -> Bool
almostEqual a b = (<2) $ count not $ zipWith (==) a b

dropAt 0 (x:xx) = xx
dropAt n [] = []
dropAt n (x:xx) = x : dropAt (n-1) xx

checksum :: [CharTable] -> Int
checksum words = countOf2 * countOf3
  where
    countOf2 = count (any (== 2) . elems) words
    countOf3 = count (any (== 3) . elems) words

type CharTable = MonoidalMap Char (Sum Int)

readCharTable :: String -> CharTable
readCharTable characters = mconcat [ [(c,1)] | c <- characters ]

count :: (a -> Bool) -> [a] -> Int
count pred aa = length (filter pred aa)

{-
 characters
 'c' -> 2
 'h' -> 1
 'a' -> 2
 'r' -> 2
 't' -> 1
 'e' -> 1
 's' -> 1

 alava
 'a' -> 3
 'l' -> 1
 'v' -> 1

  elems(alava) ---> [3,1,1]
-}
