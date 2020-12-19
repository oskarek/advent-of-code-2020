module Day9.Day9 where

import AdventPrelude.List (subsetsOfSize)
import Data.List (sort)
import qualified Day9.Parser as Parser
import Types.Solution (Solution (..))

firstDoesnt :: Int -> [Int] -> [Int] -> Maybe Int
firstDoesnt _ _ [] = Nothing
firstDoesnt subsetsSize pre (num : rest) =
  let subsets = subsetsOfSize subsetsSize pre
      sums = sum <$> subsets
   in if num `elem` sums then firstDoesnt subsetsSize (drop 1 pre ++ [num]) rest else Just num

splitWhen :: [a] -> ([a] -> Bool) -> [a] -> ([a], [a])
splitWhen acc _ [] = (acc, [])
splitWhen acc p (x : xs) =
  let accUpd = acc ++ [x]
   in if p accUpd then (accUpd, xs) else splitWhen accUpd p xs

findCont :: [Int] -> Maybe [Int]
findCont [] = Nothing
findCont nums =
  let (conts, rest) = splitWhen [] (\l -> sum l >= 22477624 && length l > 1) nums
   in if sum conts == 22477624 then Just conts else findCont (drop 1 nums)

addFirstAndLast :: [Int] -> Int
addFirstAndLast xs = head xs + last xs

part1 :: Solution
part1 =
  MkSol
    { parse = Parser.input,
      solve = uncurry (firstDoesnt 2) . splitAt 25
    }

part2 :: Solution
part2 =
  MkSol
    { parse = Parser.input,
      solve = fmap (addFirstAndLast . sort) . findCont
    }
