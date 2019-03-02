#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --package containers
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

import Data.IntSet (IntSet, member)

main :: IO ()
main = interact (
  show
  . loopFunction 0 [0]
  . cycle
  . map (  read
         . filter (/= '+'))
  . lines
  )

loopFunction :: Int -> IntSet -> [Int] -> Int
loopFunction oldFrequency seen (x:xx) =
  let newFrequency = oldFrequency + x
  in if member newFrequency seen
       then newFrequency
       else loopFunction newFrequency (seen <> [newFrequency]) xx
