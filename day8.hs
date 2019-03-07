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
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

import           Control.Category           ((>>>))
import           Control.Comonad
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable              (fold)
import qualified Data.Foldable              as F
import           Data.Function
import           Data.Generics.Labels
import           Data.Generics.Product.Typed
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
import           Text.ParserCombinators.ReadP
import           Text.Scanf

import Debug.Trace

main :: IO ()
main = interact script

script, script2 :: String -> String
script = lines >>> map (read @Entry) >>> map (foldEntry (\aa mm -> sum aa + sum mm)) >>> sum >>> show @Int
script2 = lines >>> map (read @Entry) >>> map (foldEntry valueF) >>> sum >>> show

data Entry = Entry
  { children :: ![Entry]
  , metadata :: ![Int]}
  deriving (Eq, Generic, Ord, Show)

foldEntry :: ([a] -> [Int] -> a) -> Entry -> a
foldEntry f Entry {..} = f (map (foldEntry f) children) metadata

valueF :: [Int] -> [Int] -> Int
valueF [] metadata = sum metadata
valueF cc mm = sum $ select (sort $ filter (/= 0) mm) cc

select [] _ = []
select _ [] = []
select (1 : xx) (y:yy) = y : select xx (y:yy)
select xx (_:yy) = select (map pred xx) yy

instance Read Entry where
  readsPrec _ = readP_to_S entryP

entryP = do
  num_entries <- intP
  num_metadata <-  intP
  children <- replicateM num_entries entryP
  metadata <- replicateM num_metadata intP
  return Entry{..}
  where
    intP = readS_to_P reads

example :: Entry
example = read "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
