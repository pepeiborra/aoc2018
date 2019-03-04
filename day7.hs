#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package mtl --package containers --package monoidal-containers --package hashable --package generic-lens --package lens --package scanf --package comonad
{-# LANGUAGE BangPatterns      #-}
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
{-# LANGUAGE ViewPatterns      #-}

import           Control.Category           ((>>>))
import           Control.Comonad
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable              (fold)
import qualified Data.Foldable              as F
import           Data.Function
import           Data.Graph
import           Data.Hashable
import           Data.IntMap                (IntMap, findWithDefault, fromListWith, lookup, toList)
import           Data.IntSet                (IntSet, member)
import qualified Data.IntSet as Set
import           Data.List                  (insert, insertBy, maximumBy, minimumBy, sort, sortBy,
                                             stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.Tree
import qualified Data.Semigroup             as S
import           GHC.Exts                   (fromList)
import           GHC.Generics
import           Prelude                    hiding (lookup)
import           Text.Scanf

import Debug.Trace
-- ADEFKLVQXRYPZISOTMUGHJNBWC


reformat =
  lines >>>
  map (instruction >>> (\(pre, post) -> chr pre : " -> " ++ [chr post])) >>>
  unlines

main :: IO ()
main = interact script

script, script2 :: String -> String
script = lines >>> map instruction >>> solve1 >>> map chr
script2 =
  lines >>>
  map instruction >>>
  cost 60 5 >>>
  (\(sol, log) -> unlines $ (fmap (show .fmap chr) log) ++ [show sol])

instruction :: String -> Edge
instruction txt =
  case scanf [fmt| Step %c must be finished before step %c can begin.|] txt of
    Just ( pre :+ post :+ ()) -> (ord pre, ord post)

predecessors edges n = [a | (a, b) <- edges, b == n]
successors edges n = [b | (a, b) <- edges, a == n]
nodes edges = foldMap (\(a, b) -> fromList [a, b]) edges
roots edges = Set.filter (null . predecessors edges) (nodes edges)

-- Kahn's algorithm for a topological sort using a min-Set for S
-- https://en.wikipedia.org/wiki/Topological_sorting#Algorithms
solve choose edges = execWriterT $ evalStateT (solving $ roots edges) edges
  where
    solving aaa | Set.null aaa = return ()
    solving aaa = do
      (x,aaa') <- choose aaa
      tell [x]
      edges <- get
      let ss = successors edges x
          next = fromList [n | n <- ss, [_] <- [predecessors edges n]]
      put [(a, b) | (a, b) <- edges, a /= x || b `notElem` ss]
      solving (next <> aaa')

solve1 :: [Edge] -> [Vertex]
solve1 = fromMaybe (error "no solution") . solve (maybe mzero return . Set.minView)

data CostState = CostState
  { workersNext :: [Int] -- ^ Earliest each worker can start a new task
  , itemsDone :: !IntSet
  }

data Log a = Log
  { time :: !Int
  , item :: !a
  }
  deriving (Functor)

instance Show a => Show (Log a) where
  show Log{..} = show time ++ " --> " ++ show item

cost :: Int -> Int -> [Edge] -> (Int, [Log Vertex])
cost fixedCost nworkers edges =
  runWriter $
  evalStateT (go 0 $ zip (repeat 0) $ Set.toList $ roots edges) $
  CostState (replicate nworkers 0) mempty
  where
    costOne n = n - ord 'A' + 1 + fixedCost
    go t [] = return t
    go t ((earliestStart, n):after) = do
      CostState ~(nextWorker:rest) done <- get
      if (n `member` done)
        then return t
        else do
          let c = costOne n
              startTime = max nextWorker earliestStart
              endTime = startTime + c
          tell [Log startTime n]
          put $ CostState (insert endTime rest) (done <> fromList [n])
          let more =
                [ (endTime, s)
                | s <- successors edges n
                , all (\x -> x `member` done || x == n) (predecessors edges s)
                ]
          go endTime (insertAllBy (compare `on` fst) more after)

insertAllBy f xx list = foldr (insertBy f) list xx

example =
  [
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin."
  ]

example2 =
  [
    (1, 2)
  , (1, 5)
  , (2, 3)
  , (2, 4)
  , (3, 6)
  , (4, 6)
  , (5, 6)
  ]

exampleInstructions = map instruction example
