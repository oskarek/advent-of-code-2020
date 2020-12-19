{-# LANGUAGE OverloadedStrings #-}

module Day2.Day2 where

import AdventPrelude (occurrences, xor)
import Data.Ix (inRange)
import qualified Day2.Parser as Parser
import Day2.Types (Password, Policy (..))
import Types.Solution (Solution (..))

isValid1 :: (Policy, Password) -> Bool
isValid1 (Policy lower upper char, pwd) =
  inRange (lower, upper) (occurrences char pwd)

isValid2 :: (Policy, Password) -> Bool
isValid2 (Policy idx1 idx2 char, pwd) =
  (pwd !! (idx1 - 1) == char) `xor` (pwd !! (idx2 - 1) == char)

-- Solutions

sol :: ((Policy, Password) -> Bool) -> Solution
sol isValid =
  MkSol
    { parse = Parser.input,
      solve = length . filter isValid
    }

part1, part2 :: Solution
part1 = sol isValid1
part2 = sol isValid2
