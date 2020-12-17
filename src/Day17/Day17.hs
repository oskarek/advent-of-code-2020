module Day17.Day17 where

import Control.Arrow (Arrow ((&&&)))
import Data.Map ((!), (!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Text.Megaparsec (sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, newline)
import Types.Problem (Problem (..))

-- | The state of a cube.
data CubeState = Active | Inactive deriving (Eq)

-- | Find list of adjacent points for a single point.
adjacents :: [Int] -> [[Int]]
adjacents pos = filter (/= pos) $ mapM (\d -> [d - 1, d, d + 1]) pos

-- | Add n dimensions to a map representing a matrix.
addDims :: Int -> Map.Map [Int] CubeState -> Map.Map [Int] CubeState
addDims n = Map.mapKeys (replicate n 0 ++)

-- | Transform a list represented 2d matrix into a Map representation.
mat2map :: [[CubeState]] -> Map.Map [Int] CubeState
mat2map mat = Map.fromList [([x, y], a) | (y, ys) <- zip [0 ..] mat, (x, a) <- zip [0 ..] ys]

-- | Run a single step of the simulation.
runStep :: Map.Map [Int] CubeState -> Map.Map [Int] CubeState
runStep board = Map.fromList ((fst &&& nextState) <$> withAdjacents extBoard)
  where
    extBoard = board <> Map.fromSet (const Inactive) (foldMap (Set.fromList . adjacents) (Map.keysSet board))
    nextState (pos, adjs) = case extBoard ! pos of
      Active -> if length (filter (== Active) adjs) `elem` [2, 3] then Active else Inactive
      Inactive -> if length (filter (== Active) adjs) == 3 then Active else Inactive

-- | Map together all positions with their neighbouring cubes.
withAdjacents :: Map.Map [Int] CubeState -> [([Int], [CubeState])]
withAdjacents board = (\pos -> (pos, fromMaybe Inactive . (board !?) <$> adjacents pos)) <$> Map.keys board

-- | Solve the problem, in n dimensions.
solveForDims :: Int -> [[CubeState]] -> Int
solveForDims n = length . filter (== Active) . Map.elems . (!! 6) . iterate runStep . addDims (n - 2) . mat2map

problem :: Problem
problem =
  Problem
    { parser = some ((Inactive <$ char '.') <|> (Active <$ char '#')) `sepEndBy` newline,
      solvePartOne = solveForDims 3,
      solvePartTwo = solveForDims 4
    }
