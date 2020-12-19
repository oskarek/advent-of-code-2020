module Day11.Day11 where

import AdventPrelude (idempotently, occurrences)
import Control.Arrow ((&&&))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import qualified Day11.Parser as Parser
import Day11.Types (Seat (..))
import Types.Solution (Solution (..))

-- | Find list of adjacent points for a single point.
adjacents :: [Int] -> [[Int]]
adjacents pos = filter (/= pos) $ mapM (\d -> [d - 1, d, d + 1]) pos

dirs :: [[Int]]
dirs = [[x, y] | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

-- | The immediate neighbour seats to the seat at the given position.
immediateNeighbours :: Map.Map [Int] Seat -> [Int] -> [Seat]
immediateNeighbours seats pos =
  mapMaybe (seats Map.!?) (adjacents pos)

closestNeighbours :: Map.Map [Int] Seat -> [Int] -> [Seat]
closestNeighbours map [x, y] =
  let seatsExtended = (\[x', y'] -> catMaybes $ takeWhile isJust $ (\k -> map Map.!? [x + k * x', y + k * y']) <$> [1 ..]) <$> dirs
   in mapMaybe (List.find (/= Floor)) seatsExtended

-- | Zip together all seat positions with their neighbour seats.
withNeighbours :: (Map.Map [Int] Seat -> [Int] -> [Seat]) -> Map.Map [Int] Seat -> [([Int], [Seat])]
withNeighbours findNeighbours seats = (id &&& findNeighbours seats) <$> Map.keys seats

-- | Transform a list represented 2d matrix into a Map representation.
mat2map :: [[Seat]] -> Map.Map [Int] Seat
mat2map mat = Map.fromList [([x, y], a) | (y, ys) <- zip [0 ..] mat, (x, a) <- zip [0 ..] ys]

runStep :: Int -> (Map.Map [Int] Seat -> [Int] -> [Seat]) -> Map.Map [Int] Seat -> Map.Map [Int] Seat
runStep maxNeighbCount findNeighbours seats = Map.fromList ((fst &&& nextState) <$> withNeighbours findNeighbours seats)
  where
    nextState (pos, adjs)
      | (seats Map.! pos == Empty) && Occu `notElem` adjs = Occu
      | (seats Map.! pos == Occu) && length (filter (== Occu) adjs) > maxNeighbCount = Empty
      | otherwise = seats Map.! pos

sol :: (Map.Map [Int] Seat -> Map.Map [Int] Seat) -> Solution
sol step =
  MkSol
    { parse = Parser.input,
      solve = occurrences Occu . Map.elems . idempotently step . mat2map
    }

part1, part2 :: Solution
part1 = sol (runStep 3 immediateNeighbours)
part2 = sol (runStep 4 closestNeighbours)
