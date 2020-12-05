-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day03 (day03a, day03b) where

import           AOC.Prelude

type Slope = (Int, Int)
type Grid  = [String]

day03a :: Grid :~> Int
day03a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . trees [(3, 1)]
    }

day03b :: Grid :~> Int
day03b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . trees [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    }

trees :: [Slope] -> Grid -> Int
trees slopes grid = product $ fmap (`countTrees` grid) slopes

countTrees :: Slope -> Grid -> Int
countTrees (dx, dy) grid = countTrue (=='#') [(grid !! (i * dy)) !! (i * dx `mod` width) | i <- [1 .. height], i * dy < height]
  where
    width = length $ head grid
    height = length grid 
