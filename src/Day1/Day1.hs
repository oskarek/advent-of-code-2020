module Day1.Day1 (problem, solveN) where

import AdventPrelude.List (subsetsOfSize)
import Data.List (find)
import Text.Megaparsec (many)
import qualified Types.Parser as P
import Types.Problem (Problem (..))

solveN :: Int -> [Int] -> Maybe [Int]
solveN n = find ((== 2020) . sum) . subsetsOfSize n

problem :: Problem
problem =
  Problem
    { parser = many P.int,
      solvePartOne = solveN 2,
      solvePartTwo = solveN 3
    }
