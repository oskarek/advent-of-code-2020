{-# LANGUAGE OverloadedStrings #-}

module Day2.Parser (input) where

import Day2.Types (Constraint (..), Password)
import Text.Megaparsec (anySingle, manyTill, sepBy) -- (anySingle, many, manyTill)
import Text.Megaparsec.Char (lowerChar, space, space1)
import qualified Types.Parser as P

constraint :: P.Parser Constraint
constraint = Constraint <$> P.int <* P.symbol "-" <*> P.int <*> lowerChar

password :: P.Parser Password
password = anySingle `manyTill` space1

passwordWithConstraint :: P.Parser (Constraint, Password)
passwordWithConstraint = (,) <$> constraint <* P.symbol ":" <*> password

input :: P.Parser [(Constraint, Password)]
input = passwordWithConstraint `sepBy` space
