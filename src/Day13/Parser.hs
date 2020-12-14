{-# LANGUAGE OverloadedStrings #-}

module Day13.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Types.Parser as P

mayBusId :: P.Parser (Maybe Int)
mayBusId = Just <$> P.int <|> Nothing <$ "x"

input :: P.Parser (Int, [Maybe Int])
input = (,) <$> P.int <* newline <*> (mayBusId `sepBy` P.comma)
