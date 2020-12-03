{-# LANGUAGE OverloadedStrings #-}

module Day2.Day2 where

import AdventPrelude.Bool (xor)
import AdventPrelude.List (occurrences)
import qualified Day2.Parser
import Day2.Types
import Types.Problem (Problem (..))

isValid1 :: (Constraint, Password) -> Bool
isValid1 (Constraint lower upper char, pwd) =
  let occs = occurrences char pwd
   in occs >= lower && occs <= upper

isValid2 :: (Constraint, Password) -> Bool
isValid2 (Constraint idx1 idx2 char, pwd) =
  (pwd !! (idx1 - 1) == char) `xor` (pwd !! (idx2 - 1) == char)

problem :: Problem
problem =
  Problem
    { parser = Day2.Parser.input,
      solvePartOne = length . filter isValid1,
      solvePartTwo = length . filter isValid2
    }
