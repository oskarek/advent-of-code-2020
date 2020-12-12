{-# LANGUAGE OverloadedStrings #-}

module Day12.Parser where

import Day12.Types (NavInstr (..))
import Linear (V2 (..), (^*))
import Text.Megaparsec (choice, many, (<|>))
import qualified Types.Parser as P

vect :: P.Parser (V2 Int)
vect =
  choice
    [ V2 0 1 <$ "N",
      V2 0 (-1) <$ "S",
      V2 1 0 <$ "E",
      V2 (-1) 0 <$ "W"
    ]

degreeSign :: P.Parser (Int -> Int)
degreeSign = (negate <$ "L") <|> (id <$ "R")

navInstr :: P.Parser NavInstr
navInstr =
  choice
    [ Move <$> ((^*) <$> vect <*> P.int),
      Rotate <$> (($) <$> degreeSign <*> P.int),
      Forward <$ "F" <*> P.int
    ]

navInstrs :: P.Parser [NavInstr]
navInstrs = many navInstr
