{-# LANGUAGE OverloadedStrings #-}

module Day18.Day18 where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Text.Megaparsec (sepEndBy, (<?>), (<|>))
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P
import Types.Solution (Solution (..))

expr precTable = makeExprParser (term precTable) precTable <?> "expression"

term precTable = P.lexeme (P.parens (expr precTable)) <|> P.int <?> "term"

precedenceTable1 =
  [ [ binary "+" (+),
      binary "*" (*)
    ]
  ]

precedenceTable2 =
  [ [binary "+" (+)],
    [binary "*" (*)]
  ]

-- | Parser for a left associative, infix binary operator.
binary name f = InfixL (f <$ P.symbol name)

part1 :: Solution
part1 =
  MkSol
    { parse = sum <$> (expr precedenceTable1 `sepEndBy` newline),
      solve = id
    }

part2 :: Solution
part2 =
  MkSol
    { parse = sum <$> (expr precedenceTable2 `sepEndBy` newline),
      solve = id
    }
