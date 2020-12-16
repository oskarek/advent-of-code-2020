{-# LANGUAGE OverloadedStrings #-}

module Day16.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Types.Parser as P

-- Rules

range :: P.Parser (Int, Int)
range = (,) <$> P.int <* P.symbol "-" <*> P.int

ranges :: P.Parser [(Int, Int)]
ranges = range `sepBy1` P.symbol "or"

rule :: P.Parser (String, [(Int, Int)])
rule = (,) <$> some (alphaNumChar <|> char ' ') <* P.symbol ":" <*> ranges

rules :: P.Parser [(String, [(Int, Int)])]
rules = rule `sepEndBy` newline

-- Tickets

ticket :: P.Parser [Int]
ticket = P.int `sepBy1` P.comma

myTicket :: P.Parser [Int]
myTicket = P.symbol "your ticket:" *> newline *> ticket <* newline

nearbyTickets :: P.Parser [[Int]]
nearbyTickets = P.symbol "nearby tickets:" *> newline *> (ticket `sepEndBy` newline)

-- Input

input :: P.Parser ([(String, [(Int, Int)])], [Int], [[Int]])
input = (,,) <$> (rules <* space1) <*> (myTicket <* space1) <*> nearbyTickets
