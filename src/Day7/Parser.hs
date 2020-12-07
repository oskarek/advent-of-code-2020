{-# LANGUAGE OverloadedStrings #-}

module Day7.Parser where

import qualified Data.Map as Map
import Day7.Types (Bag, BagMap)
import Text.Megaparsec (choice, count, sepBy, sepEndBy, some)
import Text.Megaparsec.Char (letterChar, newline, space1, string)
import qualified Types.Parser as P

bagWithQuantity :: P.Parser (Int, String)
bagWithQuantity = do
  qnt <- P.int
  desc <- unwords <$> count 2 (some letterChar <* space1)
  _ <- if qnt > 1 then "bags" else "bag"
  pure (qnt, desc)

bagContent :: P.Parser [(Int, String)]
bagContent =
  choice
    [ [] <$ "no other bags.",
      bagWithQuantity `sepBy` P.comma <* "."
    ]

bag :: P.Parser Bag
bag = do
  desc <- unwords <$> count 2 (some letterChar <* space1)
  _ <- string "bags contain "
  content <- bagContent
  pure (desc, content)

input :: P.Parser BagMap
input = Map.fromList <$> bag `sepEndBy` newline
