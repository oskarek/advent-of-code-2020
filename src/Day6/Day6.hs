module Day6.Day6 where

import qualified Data.Set as Set
import qualified Day6.Parser as Parser
import Types.Problem (Problem (..))

problem :: Problem
problem =
  Problem
    { parser = Parser.input,
      solvePartOne = sum . fmap (Set.size . foldr1 Set.union),
      solvePartTwo = sum . fmap (Set.size . foldr1 Set.intersection)
    }
