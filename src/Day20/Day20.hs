{-# LANGUAGE TupleSections #-}

module Day20.Day20 where

import Control.Monad.State (State, evalState, gets, modify)
import Data.Bifunctor ()
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mat
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Day20.Parser as Parser
import Day20.Types (IdTile, Tile)
import Types.Solution (Solution (..))

-- Matrix operations

-- | Rotate a matrix to the right 90 degrees.
rotateRight :: Matrix a -> Matrix a
rotateRight mat =
  let (r, c) = (Mat.nrows mat, Mat.ncols mat)
   in Mat.matrix c r $ \(i, j) -> mat Mat.! (r - j + 1, i)

-- | Flip a matrix vertically, effectively reversing all its columns.
flipVert :: Matrix a -> Matrix a
flipVert mat =
  let (r, c) = (Mat.nrows mat, Mat.ncols mat)
   in Mat.matrix c r $ \(i, j) -> mat Mat.! (r - i + 1, j)

-- | Get all possible orientations of a matrix, after rotation and/or flipping.
allOrientations :: Matrix a -> [Matrix a]
allOrientations mat =
  concatMap (take 4 . iterate rotateRight) [mat, flipVert mat]

-- | Returns true if last row of topMat equals first row of botMat.
matchesVert :: Eq a => Matrix a -> Matrix a -> Bool
matchesVert topMat botMat =
  Mat.getRow (Mat.nrows topMat) topMat == Mat.getRow 1 botMat

-- | Returns true if last column of leftMat equals first column of rightMat.
matchesHori :: Eq a => Matrix a -> Matrix a -> Bool
matchesHori leftMat rightMat =
  Mat.getCol (Mat.ncols leftMat) leftMat == Mat.getCol 1 rightMat

-- Tile assembling

type TileAssembler = State (Map Int (Matrix Char))

-- | Assemble in one direction, from the start tile, based on the areNeighbours
-- predicate.
assembleInOneDir ::
  -- | Function for assembling a single neighbour.
  (IdTile -> TileAssembler a) ->
  -- | Predicate for finding next neighbour tile.
  (Tile -> Tile -> Bool) ->
  -- | Start tile for the assembling.
  Tile ->
  TileAssembler [a]
assembleInOneDir assemble areNeighbours startTile = do
  tiles <- gets (concatMap (\(id, tile) -> (id,) <$> allOrientations tile) . Map.toList)
  case List.find (areNeighbours startTile . snd) tiles of
    Nothing -> return []
    Just idTile@(_, tile) -> do
      a <- assemble idTile
      (a :) <$> assembleInOneDir assemble areNeighbours tile

-- | Assemble the start tile and all its neighbours in the direction according
-- to the areNeighbours predicate.
assembleAll ::
  -- | Function for assembling a single unit.
  (IdTile -> TileAssembler a) ->
  -- | Predicate for finding next neighbour tile.
  (Tile -> Tile -> Bool) ->
  -- | Start tile for assembling.
  IdTile ->
  TileAssembler [a]
assembleAll assemble areNeighbours startIdTile@(_, startTile) = do
  c <- assemble startIdTile
  l <- reverse <$> assembleInOneDir assemble (flip areNeighbours) startTile
  r <- assembleInOneDir assemble areNeighbours startTile
  return (l ++ [c] ++ r)

-- | Assemble the given tile.
assembleTile :: IdTile -> TileAssembler IdTile
assembleTile tile@(tileId, _) = tile <$ modify (Map.delete tileId)

-- | Assemble the image row that contains the given tile.
assembleRow :: IdTile -> TileAssembler [IdTile]
assembleRow = assembleAll assembleTile matchesHori

-- | Assemble all rows of the image, starting at the given tile.
assembleImageRows :: IdTile -> TileAssembler [[IdTile]]
assembleImageRows = assembleAll assembleRow matchesVert

-- | Assemble all the tiles in the right positions of a matrix.
assembleTiles :: [IdTile] -> Matrix IdTile
assembleTiles (startTile : restOfTiles) =
  Mat.fromLists $ evalState (assembleImageRows startTile) (Map.fromList restOfTiles)

-- Part One

-- | Get the product of the ids of the corner tiles.
cornerIdsProduct :: Matrix IdTile -> Int
cornerIdsProduct mat =
  let (r, c) = (Mat.nrows mat, Mat.ncols mat)
      ids = fst . (mat Mat.!) <$> [(1, 1), (1, c), (r, 1), (r, c)]
   in product ids

-- Part Two

-- | Remove the border around the tile.
removeBorder :: Tile -> Tile
removeBorder mat = Mat.submatrix 2 (Mat.nrows mat - 1) 2 (Mat.ncols mat - 1) mat

-- | Remove borders and merge all tiles into the final image.
formFinalImage :: Matrix IdTile -> Matrix Char
formFinalImage mat =
  Mat.flatten (removeBorder . snd <$> mat)

-- | The positions for the pattern starting at the given position.
patternPositions :: String -> (Int, Int) -> Set.Set (Int, Int)
patternPositions patt (i, j) =
  let hashHoriPoss = List.elemIndices '#' <$> lines patt
   in Set.fromList $ concat $ zipWith (\i' -> fmap $ (i + i',) . (+ j)) [0 ..] hashHoriPoss

-- | For a given position in the image, return any positions matching a pattern
-- starting at that position.
matchedPatternPositionsAt :: String -> (Int, Int) -> Matrix Char -> Set.Set (Int, Int)
matchedPatternPositionsAt patt pos image =
  let patternPos = patternPositions patt pos
      matches = all (== Just '#') $ Set.map (\(i, j) -> Mat.safeGet i j image) patternPos
   in if matches then patternPos else Set.empty

-- | Get all the positions in the image that are part of the given pattern.
allPatternPositions :: String -> Matrix Char -> Set.Set (Int, Int)
allPatternPositions patt image =
  mconcat $ Mat.toList $ Mat.mapPos (\pos _ -> matchedPatternPositionsAt patt pos image) image

-- | The sea monster pattern.
seaMonster :: String
seaMonster =
  "                  # \n\
  \#    ##    ##    ###\n\
  \ #  #  #  #  #  #   "

-- | Get the number of hashes that are not part of a sea monster pattern.
nonSeaMonsterHashCount :: Matrix Char -> Int
nonSeaMonsterHashCount image =
  let seaMonsterPositionsForOrientations = allPatternPositions seaMonster <$> allOrientations image
      seaMonstersPositions = List.maximumBy (comparing Set.size) seaMonsterPositionsForOrientations
      totalHashCount = length $ filter (== '#') (Mat.toList image)
   in totalHashCount - Set.size seaMonstersPositions

part1 :: Solution
part1 =
  MkSol
    { parse = Parser.tiles,
      solve = cornerIdsProduct . assembleTiles
    }

part2 :: Solution
part2 =
  MkSol
    { parse = Parser.tiles,
      solve = nonSeaMonsterHashCount . formFinalImage . assembleTiles
    }
