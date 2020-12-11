module Day11.Parser where

import Day11.Types (Seat (..))
import Text.Megaparsec (sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, space1)
import qualified Types.Parser as P

input :: P.Parser [[Seat]]
input = some (Floor <$ char '.' <|> Empty <$ char 'L' <|> Occu <$ char '#') `sepEndBy` space1
