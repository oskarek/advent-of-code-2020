{-# LANGUAGE RecordWildCards #-}

module Day5.Day5 where

import AdventPrelude (zipNeighbours)
import qualified Data.List as List
import qualified Day5.Parser as Parser
import Day5.Types (Seat (..))
import Types.Problem (Problem (..))

seatID :: Seat -> Int
seatID Seat {..} = row * 8 + col

findIDGap :: [Int] -> Maybe Int
findIDGap xs = (+ 1) . fst <$> List.find hasGap (zipNeighbours xs)
  where
    hasGap (x, y) = y - x > 1

problem :: Problem
problem =
  Problem
    { parser = Parser.input,
      solvePartOne = maximum . fmap seatID,
      solvePartTwo = findIDGap . List.sort . fmap seatID
    }
