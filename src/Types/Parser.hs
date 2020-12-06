{-# LANGUAGE OverloadedStrings #-}

module Types.Parser
  ( -- * types
    ParseError,
    Parser,

    -- * parsers
    sc,
    lexeme,
    symbol,
    int,
    intInRange,
    signedInt,
    stringLiteral,
    parens,
    comma,

    -- * running the parsers
    Types.Parser.parse,
    parseAll,

    -- * error printing
    printParseError,
  )
where

import Control.Monad (guard, void)
import Data.Ix (inRange)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    between,
    empty,
    eof,
    errorBundlePretty,
    manyTill,
    parse,
  )
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

-- Type definitions

type ParseError = ParseErrorBundle Text Void

type Parser = Parsec Void Text

-- Convenience parsers

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

int :: Parser Int
int = lexeme L.decimal

intInRange :: (Int, Int) -> Parser Int
intInRange range = do
  n <- int
  guard (inRange range n)
  return n

signedInt :: Parser Int
signedInt = L.signed sc int

stringLiteral :: Parser String
stringLiteral = char '\"' *> L.charLiteral `manyTill` char '\"'

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

comma :: Parser ()
comma = void (symbol ",")

-- Running the parser

parse :: Parser a -> Text -> Either ParseError a
parse p = Text.Megaparsec.parse p ""

parseAll :: Parser a -> Text -> Either ParseError a
parseAll p = Types.Parser.parse (p <* eof)

-- Error printing

printParseError :: ParseError -> String
printParseError = Text.Megaparsec.errorBundlePretty