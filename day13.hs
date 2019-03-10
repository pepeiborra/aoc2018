#!/usr/bin/env stack
-- stack --resolver lts-13.9 script --optimize --package mtl --package containers --package monoidal-containers --package hashable --package generic-lens --package lens --package scanf --package vector --package scanf --package ansi-terminal --package array --package primitive --package unordered-containers
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

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
import           Data.List               (groupBy)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           GHC.Exts                (fromList)
import           GHC.Generics
import           Prelude                 hiding (lookup)
import           System.Environment
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  case args of
    [] -> do
      inp <- getContents
      let w = parseWorld $ lines inp
      let part1 =  (show . head . filter (not . null) . solve) w
          part2 = solve2 w
      print part1
      print part2
    [arg] -> do
      inp <- readFile arg
      let w = parseWorld (lines inp)
          loop w = do
            putStrLn $ unlines $ renderWorld w
            c <- getChar
            case c of
              'q' -> return ()
              _   -> loop (stepped $ step w)
      loop w

-- solve :: String -> [Coord]
solve = Step mempty >>> iterate (step . stepped) >>> map crashes

solve2 =
  iterate (stepped . step) >>> map carts >>>
  dropWhile ((> 1) . Map.size) >>> Map.keys . head

data Coord = C {y,x :: !Int}
 deriving (Eq, Ord, Generic, Hashable)

instance Show Coord where
  show C{..} = show (x,y)

data Dir = N | E | S | W
 deriving (Enum, Eq, Ord, Show)

moveCoord N c = c{y = y c - 1}
moveCoord E c = c{x = x c + 1}
moveCoord S c = c{y = y c + 1}
moveCoord W c = c{x = x c - 1}

data Action
  = TurnLeft
  | Straight
  | TurnRight
  deriving (Eq, Ord, Show)

action TurnRight W = N
action TurnRight x = succ x
action TurnLeft  N = W
action TurnLeft  x = pred x
action Straight  x = x

data Cart = Cart{dir :: !Dir, nextActions :: [Action]}
 deriving (Eq, Ord)

instance Show Cart where
  show Cart{..} = show dir

type Board = HashMap Coord Cell

data Cell
  = H
  | V
  | Slash
  | BackSlash
  | Cross
  deriving (Eq, Ord, Show)

updateDir :: Cell -> Cart -> Cart
updateDir H cart                       = cart
updateDir V cart                       = cart
updateDir Slash cart@Cart{dir = E}     = cart{dir = N}
updateDir Slash cart@Cart{dir = W}     = cart{dir = S}
updateDir Slash cart@Cart{dir = S}     = cart{dir = W}
updateDir Slash cart@Cart{dir = N}     = cart{dir = E}
updateDir BackSlash cart@Cart{dir = E} = cart{dir = S}
updateDir BackSlash cart@Cart{dir = W} = cart{dir = N}
updateDir BackSlash cart@Cart{dir = S} = cart{dir = E}
updateDir BackSlash cart@Cart{dir = N} = cart{dir = W}
updateDir Cross cart@Cart {nextActions = a:aa, dir} =
  cart {dir = action a dir, nextActions = aa}

data World = World
  { board :: !Board
  , carts :: !(Map Coord Cart)
  }
  deriving Show

data Step = Step
  { crashes :: !(Set Coord)
  , stepped :: World
  }

step :: World -> Step
step w = Step crashes stepped
  where
    stepped = w{carts = carts'}
    (crashes, carts') = stepCarts (board w) (carts w)

stepCarts :: Board -> Map Coord Cart -> (Set Coord, Map Coord Cart)
stepCarts b = go mempty mempty
  where
    go crashes acc (Map.minViewWithKey -> Just ((pos, cart), cc))
      | pos' `Map.member` (acc <> cc) =
        go (Set.fromList [pos'] <> crashes) (Map.delete pos' acc) (Map.delete pos' cc)
      | otherwise = go crashes ([(pos', cart')] <> acc) cc
      where
        pos' = moveCoord (dir cart) pos
        cart' = updateDir cell cart
        cell =
          case lookup pos' b of
            Just x -> x
            Nothing ->
              error $
              "Cell out of bounds: " ++
              show ("pos", pos, "dir", dir cart, "pos'", pos')
    go crashes carts _ = (crashes, carts)



instance Semigroup World where
  a <> b = World (board a <> board b) (carts a <> carts b)

instance Monoid World where
  mempty = World mempty mempty

parseWorld :: [String] -> World
parseWorld ll =
  (foldMap . foldMap)
    (uncurry parseCoord)
    [[(C{..}, c) | (x, c) <- zip [0 ..] row] | (y, row) <- zip [0 ..] ll]

parseCoord :: Coord -> Char -> World
parseCoord c '>' =
  World ( [(c, H)])
        ( [(c, Cart {dir = E, nextActions = initialActions})])
parseCoord c '<' =
  World ( [(c, H)])
        ( [(c, Cart {dir = W, nextActions = initialActions})])
parseCoord c 'v' =
  World ( [(c, V)])
        ( [(c, Cart {dir = S, nextActions = initialActions})])
parseCoord c '^' =
  World ( [(c, V)])
        ( [(c, Cart {dir = N, nextActions = initialActions})])
parseCoord c '-' = World ( [(c, H)]) mempty
parseCoord c '|' = World ( [(c, V)]) mempty
parseCoord c '+' = World ( [(c, Cross)]) mempty
parseCoord c '/' = World ( [(c, Slash)]) mempty
parseCoord c '\\' = World ( [(c, BackSlash)]) mempty
parseCoord c ' ' = World mempty mempty

initialActions = cycle [TurnLeft, Straight, TurnRight]

renderWorld :: World -> [String]
renderWorld World {..} =
    [ [ case (Map.lookup C {..} carts, lookup C {..} board) of
      (Just Cart {..}, _) -> renderDir dir
      (_, Just cell)      -> renderCell cell
      _                   -> ' '
    | x <- [0 .. maxX]
    ]
    | y <- [0 .. maxY]
    ]
  where
    maxX = maximum $ map x $ keys board
    maxY = maximum $ map y $ keys board

renderDir N = '^'
renderDir S = 'v'
renderDir W = '<'
renderDir E = '>'

renderCell H         = '-'
renderCell V         = '|'
renderCell Cross     = '+'
renderCell Slash     = '/'
renderCell BackSlash = '\\'

-- test = case solve (unlines example) of C {x = 7, y = 3} : _ -> True ; _ -> False

example :: [String]
example =
  [
    "/->-\\"
  , "|   |  /----\\"
  , "| /-+--+-\\  |"
  , "| | |  | v  |"
  , "\\-+-/  \\-+--/"
  , " \\------/  "
  ]
