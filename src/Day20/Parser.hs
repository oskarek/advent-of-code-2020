{-# LANGUAGE OverloadedStrings #-}

module Day20.Parser where

import Data.Bifunctor ()
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mat
import Day20.Types (IdTile)
import Text.Megaparsec (choice, sepBy, sepEndBy1, some)
import Text.Megaparsec.Char (char, newline)
import qualified Types.Parser as P

tileRow :: P.Parser [Char]
tileRow = some $ choice [char '.', char '#']

tileId :: P.Parser Int
tileId = "Tile " *> P.int <* ":"

tile :: P.Parser (Matrix Char)
tile = Mat.fromLists <$> (tileRow `sepEndBy1` newline)

tileWithId :: P.Parser IdTile
tileWithId = (,) <$> tileId <* newline <*> tile

tiles :: P.Parser [IdTile]
tiles = tileWithId `sepBy` newline