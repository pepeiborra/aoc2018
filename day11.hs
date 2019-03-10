#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --optimize --package mtl --package containers --package monoidal-containers --package hashable --package generic-lens --package lens --package scanf --package vector --package scanf --package ansi-terminal --package array
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
import           Control.Category   ((>>>))
import           Control.Monad
import           Data.Array.Unboxed as A
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable      (find, fold)
import qualified Data.Foldable      as F
import           Data.Function
import           Data.Hashable
import           Data.Maybe
import           Data.Semigroup
import           Data.Tuple
import           GHC.Exts           (fromList)
import           Prelude            hiding (lookup)


main :: IO ()
main = do
  unless test $ do
    print test6
    error "You fool"
  print (solve 7315)
  print (solve2 7315)

powerLevels = powerLevelsN 300

powerLevelsN :: Int -> Int -> UArray (Int,Int) Int
powerLevelsN dim serial =
  array
    ((0, 0), (dim-1, dim-1))
    [ ((x, y), ((rackId * y + serial) * rackId `div` 100 `mod` 10) - 5)
    | x <- [0 .. dim-1]
    , let rackId = x + 10
    , y <- [0 .. dim-1]
    ]

squaresRef :: Int -> UArray (Int,Int) Int -> UArray (Int, Int) Int
squaresRef ssize inp =
  array
    ((lx, ly), (hx - ssize + 1, hy - ssize + 1))
    [ ( (x, y)
      , sum
          [ inp ! (x + cx, y + cy)
          | cx <- [0 .. ssize - 1]
          , cy <- [0 .. ssize - 1]
          ])
    | x <- [lx .. hx - ssize + 1]
    , y <- [lx .. hx - ssize + 1]
    ]
  where
    ((lx, ly), (hx, hy)) = bounds inp

squares ssize inp = iterate (step inp) inp !! (ssize - 1)

step :: UArray (Int, Int) Int -> UArray (Int,Int) Int -> UArray (Int, Int) Int
step inp squares =
  array
    ((lx, ly), (hx - 1, hy - 1))
    [ ( (x, y)
      , squares ! (x, y) +
        sum [inp ! (x + cx, y + dist) | cx <- [0 .. dist - 1]] +
        sum [inp ! (x + dist, y + cy) | cy <- [0 .. dist - 1]] +
        inp ! (x + dist, y + dist))
    | x <- [lx .. hx - 1]
    , y <- [ly .. hy - 1]
    ]
  where
    dist = hy' - hy + 1
    ((lx, ly), (hx, hy)) = bounds squares
    ((_, _), (_, hy')) = bounds inp

solve :: Int -> (Int,Int,Int)
solve serial = (m,x,y)
  where
    grid = squares 3 (powerLevels serial)
    ((x,y),m) = F.maximumBy (compare `on` snd) (assocs grid)

solve2 :: Int -> (Int,Int,Int)
solve2 serial = (x, y, s)
  where
    powers = powerLevels serial
    ((s, x, y), _) =
      F.maximumBy
        (compare `on` snd)
        [ ((ssize, x, y), m)
        | (ssize,grid) <- zip [1 .. 297] $ iterate (step powers) powers
        , let ((x, y), m) = F.maximumBy (compare `on` snd) (assocs grid)
        ]

test0 = powerLevels 8 ! (3,5)
test1 = powerLevels 57 ! (122,79)
test2 = powerLevels 39 ! (217,196)
test3 = powerLevels 71 ! (101,153)
test4 = squares 3 (powerLevels 42) ! (21,61)
test5 = solve 42
test6 = solve2 18
test7 = solve2 42
test8 = squares 3 (powerLevels 42) == squaresRef 3 (powerLevels 42)

test =
  and
    [ test0 == 4
    , test1 == -5
    , test2 == 0
    , test3 == 4
    , test4 == 30
    , test5 == (30, 21, 61)
    , test6 == (90,269,16)
    , test7 == (232,251,12)
    , test8
    ]

render g =
  unlines [unwords [renderOne (g ! (x, y)) | x <- [lx .. mx]] | y <- [ly .. my]]
  where
    renderOne x
      | x >= 0 = ' ' : show x
      | otherwise = show x
    ((lx, ly), (mx, my)) = bounds g
