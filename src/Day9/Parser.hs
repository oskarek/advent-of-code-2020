module Day9.Parser where

import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P

input :: P.Parser [Int]
input = P.int `sepEndBy` newline
