{-# LANGUAGE OverloadedStrings #-}

module Day24.Day24 where

import AdventPrelude (applyNTimes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Linear (V2 (..))
import Text.Megaparsec (choice, sepEndBy, some)
import Text.Megaparsec.Char (newline, string)
import qualified Types.Parser as P
import Types.Solution (Solution (..))

-- Types

type TilePos = V2 Int

-- | Map the tile position instruction to the corresponding vector.
dirs :: [(Text, V2 Int)]
dirs =
  [ ("e", V2 2 0),
    ("se", V2 1 (-1)),
    ("sw", V2 (-1) (-1)),
    ("w", V2 (-2) 0),
    ("nw", V2 (-1) 1),
    ("ne", V2 1 1)
  ]

-- Parsing

dirParser :: P.Parser (V2 Int)
dirParser = choice $ (\(name, vec) -> vec <$ string name) <$> dirs

tilePosParser :: P.Parser TilePos
tilePosParser = sum <$> some dirParser

-- Solve logic

-- | Get the tile position along with its neighbour positions.
tileWithNeighbours :: TilePos -> Set TilePos
tileWithNeighbours tile = Set.insert tile (neighbours tile)

-- | Get the neighbour positions to the given tile position.
neighbours :: TilePos -> Set TilePos
neighbours tile = Set.fromList ((+ tile) . snd <$> dirs)

-- | Toggle the precence of the element in the set.
toggle :: Ord a => a -> Set a -> Set a
toggle a s = if a `Set.member` s then Set.delete a s else Set.insert a s

-- | Let one day pass, and update which tiles are black.
day :: Set TilePos -> Set TilePos
day blacks = Set.filter shouldBeBlack (foldMap tileWithNeighbours blacks)
  where
    isBlack = (`Set.member` blacks)
    blackNeighbCount = Set.size . Set.filter isBlack . neighbours
    shouldBeBlack tilePos
      | isBlack tilePos = blackNeighbCount tilePos `elem` [1, 2]
      | otherwise = blackNeighbCount tilePos == 2

part1 :: Solution
part1 =
  MkSol
    { parse = tilePosParser `sepEndBy` newline,
      solve = Set.size . foldr toggle Set.empty
    }

part2 :: Solution
part2 =
  MkSol
    { parse = tilePosParser `sepEndBy` newline,
      solve = Set.size . applyNTimes 100 day . foldr toggle Set.empty
    }
