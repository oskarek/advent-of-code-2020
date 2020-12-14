{-# LANGUAGE OverloadedStrings #-}

module Day2.Day2 where

import AdventPrelude (occurrences, xor)
import Data.Ix (inRange)
import qualified Day2.Parser
import Day2.Types
import Types.Problem (Problem (..))

isValid1 :: (Policy, Password) -> Bool
isValid1 (Policy lower upper char, pwd) =
  inRange (lower, upper) (occurrences char pwd)

isValid2 :: (Policy, Password) -> Bool
isValid2 (Policy idx1 idx2 char, pwd) =
  (pwd !! (idx1 - 1) == char) `xor` (pwd !! (idx2 - 1) == char)

problem :: Problem
problem =
  Problem
    { parser = Day2.Parser.input,
      solvePartOne = length . filter isValid1,
      solvePartTwo = length . filter isValid2
    }
