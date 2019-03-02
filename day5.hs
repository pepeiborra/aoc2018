#!/usr/bin/env stack
-- stack --resolver lts-13.9 script
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

import           Control.Category ((>>>))
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable    (fold)
import           Data.Function
import           Data.List        (minimumBy, sort, stripPrefix)
import           Data.Monoid
import           Prelude          hiding (lookup)

example = "dabAcCaCBAcCcaDA"

main :: IO ()
main = interact script2

script, script2 :: String -> String
script = show . length . reduce . filter isAlpha

reduce :: String -> String
reduce = foldr f [] where
  f c (x:acc) | toLower c == toLower x && x /= c = acc
  f c acc = c:acc

script2 = show . snd . minimumBy (compare `on` snd) . test2

test2 input =
  [(c, length (reduce (filter (not . unit c) input'))) | c <- ['a' .. 'z']]
  where
    input' = filter isAlpha input
    unit c x = toLower x == c
