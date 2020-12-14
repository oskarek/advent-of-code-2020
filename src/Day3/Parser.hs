module Day3.Parser (input) where

import Day3.Types
import Text.Megaparsec (many, sepEndBy, (<|>))
import Text.Megaparsec.Char (char, newline)
import qualified Types.Parser as P

mapPos :: P.Parser MapPos
mapPos = (Tree <$ char '#') <|> (Free <$ char '.')

mapRow :: P.Parser [MapPos]
mapRow = cycle <$> many mapPos

input :: P.Parser TreeMap
input = mapRow `sepEndBy` newline
