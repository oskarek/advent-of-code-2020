module Day7.Day7 where

import Data.Map (keys, (!?))
import Data.Maybe (fromMaybe)
import qualified Day7.Parser as Parser
import Day7.Types (BagMap)
import Types.Problem (Problem (..))

contains :: BagMap -> String -> String -> Bool
contains bagMap subBag bag =
  let names = snd <$> fromMaybe [] (bagMap !? bag)
      directlyContains = subBag `elem` names
      recursivelyContains = or (contains bagMap subBag <$> names)
   in directlyContains || recursivelyContains

contentCount :: BagMap -> String -> Int
contentCount bagMap bag =
  let content = fromMaybe [] (bagMap !? bag)
   in sum (f <$> content)
  where
    f (qnt, name) = qnt + qnt * contentCount bagMap name

problem :: Problem
problem =
  Problem
    { parser = Parser.input,
      solvePartOne = \bagMap -> length $ filter (contains bagMap "shiny gold") (keys bagMap),
      solvePartTwo = (`contentCount` "shiny gold")
    }
