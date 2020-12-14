{-# LANGUAGE OverloadedStrings #-}

module Day2.Parser (input) where

import Day2.Types (Password, Policy (..))
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (lowerChar, space1)
import qualified Types.Parser as P

policy :: P.Parser Policy
policy = Policy <$> P.int <* P.symbol "-" <*> P.int <*> lowerChar

password :: P.Parser Password
password = some lowerChar

passwordWithPolicy :: P.Parser (Policy, Password)
passwordWithPolicy = (,) <$> policy <* P.symbol ":" <*> password

input :: P.Parser [(Policy, Password)]
input = passwordWithPolicy `sepEndBy` space1
