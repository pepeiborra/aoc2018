#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --optimize --package mtl --package containers --package monoidal-containers --package hashable --package generic-lens --package lens --package scanf --package vector --package scanf --package ansi-terminal --package array --package primitive
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST.Strict
import           Data.Char
import           Data.Foldable       (find, fold)
import qualified Data.Foldable       as F
import           Data.Function
import           Data.Hashable
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Set            as Set
import           Data.Tuple
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V
import           GHC.Exts            (fromList)
import           Prelude             hiding (lookup)
import           System.Environment

main :: IO ()
main = do
  [nturns] <- getArgs
  let n :: Int = read nturns
  ex <- parse . lines <$> getContents
  print $
    if n > 1000000
      then let base = uncurry getTotal $ uncurry (solveInPlace 1000000) ex
               inc = uncurry getTotal (uncurry (solveInPlace 1000001) ex) - base
           in base + fromIntegral (n - 1000000) * inc
      else uncurry getTotal $ uncurry (solveInPlace n) ex

represent turns total trace =
  unlines $
  [ unwords
    [ show ix
    , show $ getTotal turns row
    , ":"
    , render row
    ]
  | (ix, row) <- zip [0..] trace
  ]

render row =
  [ if x
    then '#'
    else '.'
  | x <- V.toList row
  ]

getTotal :: Int -> Vector Bool -> Integer
getTotal offset =
  V.ifoldl'
    (\acc i x ->
       if x
         then acc + fromIntegral (i - offset)
         else acc)
    0

solve :: Int -> [Bool] -> Set.Set (Vector Bool) -> (Integer, [Vector Bool])
solve turns initial patterns = (total, trace)
  where
    total = getTotal 21 (last trace)
    trace = take (turns + 1) $ iterate (step match) board
    match x = Set.member x patterns
    board = pad <> V.fromList initial <> pad
    pad = V.generate 21 (const False)

findLast b = head [ x | x <- [V.length b - 1,V.length b - 2 .. 0], b ! x ]

solveInPlace :: Int -> [Bool] -> Set.Set (Vector Bool) -> (Int, Vector Bool)
solveInPlace turns initial patterns =
  runST $ do
    let pad = V.generate 20 (const False)
        board = pad <> V.fromList initial <> pad
    board <- V.unsafeThaw board
    plate <- VM.replicate 5 False
    let loop offset 0 board = return (offset, board)
        loop offset n board = do
          b <- V.unsafeFreeze board
          let Just firstPlant = V.findIndex id b
              lastPlant = findLast b
              dim = lastPlant - firstPlant
              slack = firstPlant + V.length b - lastPlant
          (board, slack) <-
            if slack < 16
              then (, slack + V.length b) <$> VM.new (V.length b * 2)
              else return (board, slack)
          let offset'
                | firstPlant < 8 = offset + ((slack - firstPlant) `div` 2)
                | lastPlant >= VM.length board - 8 =
                  offset - ((slack - V.length b + lastPlant) `div` 2)
                | otherwise = offset
              adj x = x - offset + offset'
              go i = do
                p <- V.unsafeFreeze plate
                VM.unsafeWrite plate 0 (p ! 1)
                VM.unsafeWrite plate 1 (p ! 2)
                VM.unsafeWrite plate 2 (p ! 3)
                VM.unsafeWrite plate 3 (p ! 4)
                VM.unsafeWrite plate 4 ((i + 2 < V.length b) && b ! (i + 2))
                VM.write board (adj i) (p `Set.member` patterns)
              goBack i = do
                p <- V.unsafeFreeze plate
                VM.unsafeWrite plate 4 (p ! 3)
                VM.unsafeWrite plate 3 (p ! 2)
                VM.unsafeWrite plate 2 (p ! 1)
                VM.unsafeWrite plate 1 (p ! 0)
                VM.unsafeWrite plate 0 (i >= 2 && b ! (i - 2))
                VM.write board (adj i) (p `Set.member` patterns)
          VM.unsafeWrite plate 0 False
          VM.unsafeWrite plate 1 False
          VM.unsafeWrite plate 2 False
          VM.unsafeWrite plate 3 False
          VM.unsafeWrite plate 4 False
          if adj 0 <= 0
            then mapM_ go     [firstPlant - 2 .. lastPlant + 2]
            else mapM_ goBack [lastPlant + 2, lastPlant + 1 .. firstPlant - 2]
          erase firstPlant (adj firstPlant - 3) board
          erase (adj lastPlant + 3) lastPlant board
          loop offset' (n - 1) board
    (offset, board) <- loop 20 turns board
    b <- V.unsafeFreeze board
    return (offset, b)

erase from to b = forM_ [from .. to] $ \i -> VM.write b i False

padding = 5

parse :: [String] -> ([Bool], Set.Set (Vector Bool))
parse (initialStateString:_:patternStrings) = (initial, patterns)
  where
    patterns =
      Set.fromList
        [ V.fromList [x == '#' | x <- take 5 l]
        | l <- patternStrings
        , l !! 9 == '#'
        ]
    initial = [x == '#' | x <- drop 15 initialStateString]

step :: (Vector Bool -> Bool) -> Vector Bool -> Vector Bool
step match board =
  board V.//
  [ (i, match (V.slice (i - 2) 5 board))
  | i <- [firstPlant - 2 .. lastPlant + 2]
  ]
  where
    Just firstPlant = V.findIndex id board
    Just lastPlant =
      (pred (V.length board) -) <$> V.findIndex id (V.reverse board)

test1 = uncurry (solve 20) (parse example)
test2 = uncurry getTotal $ uncurry (solveInPlace 20) (parse example)
testN n =
  fst (uncurry (solve n) (parse example)) ==
  uncurry getTotal (uncurry (solveInPlace n) (parse example))

test = and [fst test1 == 325, test2 == 325]

example =
  [ "initial state: #..#.#..##......###...###"
  , ""
  , "...## => #"
  , "..#.. => #"
  , ".#... => #"
  , ".#.#. => #"
  , ".#.## => #"
  , ".##.. => #"
  , ".#### => #"
  , "#.#.# => #"
  , "#.### => #"
  , "##.#. => #"
  , "##.## => #"
  , "###.. => #"
  , "###.# => #"
  , "####. => #"
  ]
