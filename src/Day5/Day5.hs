{-# LANGUAGE RecordWildCards #-}

module Day5.Day5 where

import AdventPrelude (zipNeighbours)
import qualified Data.List as List
import qualified Day5.Parser as Parser
import Day5.Types (Seat (..))
import Safe (maximumMay)
import Types.Solution (Solution (..))

seatID :: Seat -> Int
seatID Seat {..} = row * 8 + col

findIDGap :: [Int] -> Maybe Int
findIDGap xs = (+ 1) . fst <$> List.find hasGap (zipNeighbours xs)
  where
    hasGap (x, y) = y - x > 1

-- Solutions

sol :: ([Int] -> Maybe Int) -> Solution
sol solveForSeatIDs =
  MkSol
    { parse = Parser.input,
      solve = solveForSeatIDs . fmap seatID
    }

part1, part2 :: Solution
part1 = sol maximumMay
part2 = sol (findIDGap . List.sort)
