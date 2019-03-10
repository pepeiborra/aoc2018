#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --optimize --package mtl --package containers --package monoidal-containers --package hashable --package generic-lens --package lens --package scanf --package vector --package scanf --package ansi-terminal --package array --package primitive --package unordered-containers
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

import           Control.Arrow
import           Control.Category        ((>>>))
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST.Strict
import           Data.Char
import           Data.Foldable           (find, fold)
import qualified Data.Foldable           as F
import           Data.Function
import           Data.Hashable
import           Data.HashMap.Strict     (HashMap, keys, lookup, (!))
import           Data.IORef
import           Data.List               (groupBy, unfoldr)
import           Data.List.NonEmpty      (NonEmpty (..), nonEmpty)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Sequence           (Seq (..), (!?))
import qualified Data.Sequence           as Seq
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           GHC.Exts                (fromList)
import           GHC.Generics
import           Prelude                 hiding (lookup)
import           System.Environment
import           System.IO
import           Text.Printf

{-Options for data structure:
   - Mutable double linked-list
      - can have pointers to intermediate parts of the list
      - fast appends
      - optimal choice
   - Finger tree
      - fast appends
      - fast indexing
      - available in containers
 -}

input = 409551

main :: IO ()
main = do
  iter <- solve input
  print iter
  print =<< solve2 (digits input)

  -- | Node in a doubly linked circular mutable list
data Node = Node
  { value      :: !Int
  , next, prev :: !(IORef Node)
  }
  deriving Eq

-- | Returns a pointer to the first element of the DLL containing the values of the input list
fromListDLL :: NonEmpty Int -> IO Node
fromListDLL (a :| rest) = mdo
  ref <- newIORef initial
  let initial = Node a ref ref
  _ <- insertAll rest initial
  return initial

-- | @takeL n@ returns the previous n elements in visit order
takeL :: Int -> Node -> IO [Int]
takeL 0 _ = return []
takeL n p = (value p :) <$> (takeL (n-1) =<< readIORef (prev p))

-- | @insert value p@ extends the DLL with a new element after @p@
insert :: Int -> Node -> IO Node
insert value p@Node{next=np} = do
  next <- readIORef (next p) >>= newIORef
  prev <- newIORef p
  let this = Node{..}
  writeIORef np this
  return this

insertAll :: [Int] -> Node -> IO Node
insertAll values p = foldM (flip insert) p values

moveForward :: Int -> Node -> IO Node
moveForward 0 = return
moveForward n = readIORef . next >=> moveForward (n-1)

moveBack :: Int -> Node -> IO Node
moveBack 0 = return
moveBack n = readIORef . prev >=> moveBack (n-1)

data State = S
  { first, second, head, last :: !Node
  , count                     :: !Int
  }

render :: State -> IO String
render S {..} = go head
  where
    go n = do
      let rep
            | n == first = printf "(%d)" (value n)
            | n == second = printf "[%d]" (value n)
            | otherwise = printf " %d " (value n)
      if n == last
        then return rep
        else do
          rest <- readIORef (next n) >>= go
          return $ rep ++ rest

initial :: IO State
initial = do
  head <- fromListDLL [3,7]
  second <- readIORef (next head)
  let first = head
      last  = second
      count = 2
  return S{..}

step :: State -> IO State
step S {..} = do
  let nextSum = value first + value second
      nextRecipes
        | nextSum > 9 =
          let (d, m) = divMod nextSum 10
          in [d, m]
        | otherwise = [nextSum]
  last <- insertAll nextRecipes last
  first <- moveForward (value first + 1) first
  second <- moveForward (value second + 1) second
  count <- return $ count + length nextRecipes
  return S {..}

iterateN 0 f x = return x
iterateN n f x = f x >>= iterateN (n-1) f

solve :: Int -> IO Int
solve iterations = do
  let loop st
        | count st >= iterations + 10 = return st
        | otherwise = step st >>= loop
  final <- loop =<< initial
  let newIterationsCount = count final - iterations
  digits <-
    drop (newIterationsCount - 10) <$>
    takeL newIterationsCount (Main.last final)
  return $ foldr (\x acc -> x + 10 * acc) 0 digits

solve2 needle = do
  h  <- initial
  st <- iterateN (length needle) step h
  let loop st = do
        st' <- step st
        p7 <- moveBack 7 (Main.last st')
        done <- isPrefix needle p7
        done2 <- isPrefix needle =<< readIORef (next p7)
        if done then return (count st' - 8) else
          if done2 then return (count st' - 7) else do
            loop st'
  loop st

digits :: Int -> [Int]
digits = reverse . unfoldr f
  where
    f 0 = Nothing
    f n = let (d,m) = divMod n 10 in Just (m, d)

findSequence :: [Int] -> (Node -> Bool) -> Node -> IO (Maybe (Int, Node))
findSequence needle terminate = go 0
  where
    go n haystack =
      if terminate haystack
        then return Nothing
        else do
          done <- isPrefix needle haystack
          if done
            then return $ Just (n, haystack)
            else go (n+1) =<< readIORef (next haystack)

isPrefix :: [Int] -> Node -> IO Bool
isPrefix [] _ = return True
isPrefix (a:aa) Node{..}
  | a == value = isPrefix aa =<< readIORef next
  | otherwise  = return False

test1 = solve 5
test2 = solve 9
test3 = solve 18
test4 = solve 2018
test5 = solve2 (digits 51589)
test6 = solve2 (0 : digits 01245)
test7 = solve2 (digits 92510)
test8 = solve2 (digits 59414)

test :: IO Bool
test = and <$> sequence
  ([ (== 0124515891) <$> test1
   , (== 5158916779) <$> test2
   , (== 9251071085) <$> test3
   , (== 5941429882) <$> test4
   , (== 9) <$> test5
   , (== 5) <$> test6
   , (== 18) <$> test7
   , (== 2018) <$> test8
   ] :: [_])
