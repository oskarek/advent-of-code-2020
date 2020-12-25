module Day16.Day16 where

import AdventPrelude
import Data.Ix (inRange)
import Data.List (isPrefixOf, transpose)
import qualified Data.Set as Set
import qualified Day16.Parser as Parser
import Types.Solution (Solution (..))

-- | Returns true if the field matches the rule.
fieldMatchesRule :: Int -> (String, [(Int, Int)]) -> Bool
fieldMatchesRule field (_, ruleRanges) = any (`inRange` field) ruleRanges

-- | Returns true if the field is valid (i.e. matches at least one of the rules).
fieldIsValid :: Int -> [(String, [(Int, Int)])] -> Bool
fieldIsValid field = any (fieldMatchesRule field)

-- | The sum of all the invalid ticket fields.
ticketScanningRate :: [(String, [(Int, Int)])] -> [Int] -> Int
ticketScanningRate rules = sum . filter (not . (`fieldIsValid` rules))

-- | Returns true if the ticket is valid
ticketIsValid :: [Int] -> [(String, [(Int, Int)])] -> Bool
ticketIsValid ticket rules = all (`fieldIsValid` rules) ticket

-- | Returns true if all the fields match the given rules.
allFieldsMatch :: [Int] -> (String, [(Int, Int)]) -> Bool
allFieldsMatch fields rule = all (`fieldMatchesRule` rule) fields

-- | Get, for each index in you own ticket, all the possible fields they could represent.
possibleFieldsPerIndex :: [(String, [(Int, Int)])] -> [[Int]] -> [(Int, Set.Set String)]
possibleFieldsPerIndex rules tickets =
  let possibleFields = Set.fromList . fmap fst . matchingRules <$> transpose tickets
   in zip [0 ..] possibleFields
  where
    matchingRules fields = filter (allFieldsMatch fields) rules

-- Solvers

solve1 :: ([(String, [(Int, Int)])], [Int], [[Int]]) -> Int
solve1 (rules, _, nearbyTickets) = ticketScanningRate rules (concat nearbyTickets)

solve2 :: ([(String, [(Int, Int)])], [Int], [[Int]]) -> Maybe Int
solve2 (rules, myTicket, nearbyTickets) = do
  let validsNearby = filter (`ticketIsValid` rules) nearbyTickets
  indexedFields <- uniqueMapping (possibleFieldsPerIndex rules validsNearby)
  let depIndices = fst <$> filter (("departure" `isPrefixOf`) . snd) indexedFields
  return $ product ((myTicket !!) <$> depIndices)

part1 :: Solution
part1 =
  MkSol
    { parse = Parser.input,
      solve = solve1
    }

part2 :: Solution
part2 =
  MkSol
    { parse = Parser.input,
      solve = solve2
    }
