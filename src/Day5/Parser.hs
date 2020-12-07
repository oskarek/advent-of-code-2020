module Day5.Parser where

import Data.Char (digitToInt)
import Data.List (foldl')
import Day5.Types (Seat (..))
import Text.Megaparsec (count, sepEndBy, (<|>))
import Text.Megaparsec.Char (char, newline)
import qualified Types.Parser as P

-- | Turn a String, representing a binary number, into an Int.
binToDec :: String -> Int
binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

seat :: P.Parser Seat
seat = do
  row <- binToDec <$> count 7 ('0' <$ char 'F' <|> '1' <$ char 'B')
  col <- binToDec <$> count 3 ('0' <$ char 'L' <|> '1' <$ char 'R')
  pure $ Seat row col

input :: P.Parser [Seat]
input = seat `sepEndBy` newline