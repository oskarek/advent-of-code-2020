module Day3.Day3 where

import AdventPrelude (occurrences, takeEvery)
import qualified Day3.Parser as Parser
import Day3.Types (MapPos (..), Slope, TreeMap)
import Types.Problem (Problem (..))

-- | Get the positions in the map that lie along the slope.
slopePositions :: Slope -> TreeMap -> [MapPos]
slopePositions (dx, dy) =
  zipWith (flip (!!)) [0, dx ..] . takeEvery dy

-- | Get the number of trees that lie along the slope.
treeCountOnSlope :: Slope -> TreeMap -> Int
treeCountOnSlope slope =
  occurrences Tree . slopePositions slope

-- | Solve the problem for the given slopes.
solveForSlopes :: [Slope] -> TreeMap -> Int
solveForSlopes slopes map =
  product (flip treeCountOnSlope map <$> slopes)

problem :: Problem
problem =
  Problem
    { parser = Parser.input,
      solvePartOne = solveForSlopes [(3, 1)],
      solvePartTwo = solveForSlopes [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    }
