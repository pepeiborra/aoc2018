#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --optimize --package mtl --package containers --package monoidal-containers --package hashable --package generic-lens --package lens --package scanf --package vector --package deepseq
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
import           Control.DeepSeq
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable              (find, fold)
import qualified Data.Foldable              as F
import           Data.Function
import           Data.Graph
import           Data.Hashable
import           Data.Maybe
import           Data.Monoid
import           Data.Tree
import qualified Data.Semigroup             as S
import           Data.Vector.Unboxed        as V (Vector, replicate, accum, (!), maximum)
import           GHC.Exts                   (fromList)
import           Prelude                    hiding (lookup)

import Debug.Trace

main :: IO ()
main = do
  print $ part1
  print $ part2

part1 = solve 448 71628
part2 = solve 448 7162800

solve :: Int -> Int -> Int
solve nplayers nturns = V.maximum $ getScore $ generate nplayers !! nturns

data Generator = G
  { player, turn :: !Int
  , score :: [(Int, Int)]
  , compress :: !(Vector Int)
  , board :: !(Zipper Int)
  }

getScore :: Generator -> Vector Int
getScore G{..} = accum (+) compress score

initial nplayers = G 1 1 [] (V.replicate nplayers 0) (Z [] [0])

generate :: Int -> [Generator]
generate nplayers = iterate (compress . go) (initial nplayers)
  where
    compress g
      | turn g `mod` 2^10 == 0 =
        g { score = []
          , compress = getScore g}
      | otherwise = g
    go :: Generator -> Generator
    go G{..} =
        let (lastMove, board') = move board turn in
          G { player = succ player `mod` nplayers
            , turn = succ turn
            , score = if lastMove == 0 then score else (player, lastMove) : score
            , board = board'
            , ..
            }

data Zipper a = Z [a] [a]

instance Show a => Show (Zipper a) where show = render

render (Z [] []) = ""
render (Z pre [])  = unwords $ map show $ reverse pre
render (Z pre (cur:post)) =
    unwords (map show (reverse pre)) ++ " [" ++ show cur ++ "] " ++ unwords (map show post)

move :: Zipper Int -> Int -> (Int, Zipper Int)
move z x | x `mod` 23 == 0 = let (a,z') = specialMove z in (a+x, z')
move z x = (0, standardMove z x)

standardMove :: Zipper a -> a -> Zipper a
standardMove (Z pre (cur:next:post)) a = Z (next:cur:pre) (a:post)
standardMove (Z pre [cur]) a = let h:t = reverse (cur:pre) in Z [h] (a:t)

specialMove :: Zipper a -> (a, Zipper a)
specialMove (Z (x1:x2:x3:x4:x5:x6:a:pre) post) = (a, Z pre (x6:x5:x4:x3:x2:x1:post))
specialMove (Z pre post) = (a, Z pre' post') where
  lpre = length pre
  (post1, a:post2) = splitAt (length post - 7 + lpre ) post
  pre' = reverse post1 ++ pre
  post' = post2

test1 = solve 10 1618 == 8317
test2 = solve 13 7999 == 146373
test3 = solve 17 1104 == 2764
test4 = solve 21 6111 == 54718
test5 = solve 30 5807 == 37305
