{-# LANGUAGE OverloadedStrings #-}

module Day14.Parser where

import Day14.Types (BitMaskInstr (..), MaskBit (..))
import Text.Megaparsec (choice, sepEndBy, some)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Types.Parser as P

maskBit :: P.Parser MaskBit
maskBit = choice [Zero <$ "0", One <$ "1", X <$ "X"]

bitMaskInstr :: P.Parser BitMaskInstr
bitMaskInstr =
  choice
    [ SetMask <$> ("mask = " *> some maskBit),
      WriteToMem <$> ("mem[" *> P.int <* "] = ") <*> L.decimal
    ]

input :: P.Parser [BitMaskInstr]
input = bitMaskInstr `sepEndBy` space1
