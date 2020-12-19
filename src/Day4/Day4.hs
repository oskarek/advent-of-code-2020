{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day4.Day4 where

import Data.Map (keysSet, (!?))
import Data.Maybe (isJust)
import Data.Set (member)
import qualified Day4.Parser as Parser
import Day4.Types (EntryMap, Passport (..))
import Text.Megaparsec (parseMaybe)
import Types.Parser (intInRange)
import Types.Solution (Solution (..))

-- Part One

hasAllRequiredEntries :: EntryMap -> Bool
hasAllRequiredEntries m =
  all (`member` keysSet m) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- Part Two

makePassport :: EntryMap -> Maybe Passport
makePassport entries = do
  byr <- intInRange (1920, 2002) `parsedAt` "byr"
  iyr <- intInRange (2010, 2020) `parsedAt` "iyr"
  eyr <- intInRange (2020, 2030) `parsedAt` "eyr"
  hgt <- Parser.height `parsedAt` "hgt"
  hcl <- Parser.color `parsedAt` "hcl"
  ecl <- Parser.eyeColor `parsedAt` "ecl"
  pid <- Parser.passportID `parsedAt` "pid"
  let cid = Parser.countryID `parsedAt` "cid"
  pure Passport {..}
  where
    parser `parsedAt` key = entries !? key >>= parseMaybe parser

-- Solutions

sol :: (EntryMap -> Bool) -> Solution
sol entryMapPred =
  MkSol
    { parse = Parser.input,
      solve = length . filter entryMapPred
    }

part1, part2 :: Solution
part1 = sol hasAllRequiredEntries
part2 = sol (isJust . makePassport)
