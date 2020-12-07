{-# LANGUAGE RecordWildCards #-}

module Day5.Day5 where

import qualified Data.List as L
import qualified Day5.Parser as Parser
import Day5.Types (Seat (..))
import Types.Problem (Problem (..))

seatID :: Seat -> Int
seatID Seat {..} = row * 8 + col

findIDGap :: [Int] -> Maybe Int
findIDGap xs = (+ 1) . fst <$> L.find hasGap (zip xs (drop 1 xs))
  where
    hasGap (x, y) = y - x > 1

problem :: Problem
problem =
  Problem
    { parser = Parser.input,
      solvePartOne = L.maximum . fmap seatID,
      solvePartTwo = findIDGap . L.sort . fmap seatID
    }
