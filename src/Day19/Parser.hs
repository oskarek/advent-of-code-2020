{-# LANGUAGE OverloadedStrings #-}

module Day19.Parser where

import qualified Data.Map as Map
import Day19.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Types.Parser as P

messageRule :: P.Parser Rule
messageRule = (Ch <$> (char '\"' *> alphaNumChar <* char '\"')) <|> (Choice <$> (many P.int `sepBy` P.symbol "|"))

indexedMessageRule :: P.Parser (Int, Rule)
indexedMessageRule = (,) <$> P.int <* P.symbol ":" <*> messageRule

indexedMessageRules :: P.Parser (Map.Map Int Rule)
indexedMessageRules = Map.fromList <$> (indexedMessageRule `sepEndBy1` newline)

message :: P.Parser String
message = some alphaNumChar

messages :: P.Parser [String]
messages = message `sepEndBy1` newline

input :: P.Parser (Map.Map Int Rule, [String])
input = (,) <$> indexedMessageRules <* newline <*> messages