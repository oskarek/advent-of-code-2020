{-# LANGUAGE OverloadedStrings #-}

module Day8.Parser where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Day8.Types (BootCodeInstr (..), Op (..))
import Text.Megaparsec (choice, sepEndBy)
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P

op :: P.Parser Op
op = choice [Acc <$ "acc", Jmp <$ "jmp", Nop <$ "nop"]

instr :: P.Parser BootCodeInstr
instr = BootCodeInstr <$> P.lexeme op <*> P.signedInt

instrs :: P.Parser (Seq BootCodeInstr)
instrs = (|> End) . Seq.fromList <$> instr `sepEndBy` newline
