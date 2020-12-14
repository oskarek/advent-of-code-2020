module Day1.Day1 (problem, solveN) where

import AdventPrelude (subsetsOfSize)
import Data.List (find)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P
import Types.Problem (Problem (..))

solveN :: Int -> [Int] -> Maybe [Int]
solveN n = find ((== 2020) . sum) . subsetsOfSize n

problem :: Problem
problem =
  Problem
    { parser = P.int `sepEndBy` newline,
      solvePartOne = fmap product . solveN 2,
      solvePartTwo = fmap product . solveN 3
    }
