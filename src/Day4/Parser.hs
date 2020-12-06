{-# LANGUAGE OverloadedStrings #-}

module Day4.Parser where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Day4.Types (Color (..), EntryMap, EyeColor (..), Height (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Types.Parser as P

-- Entry maps

passportEntry :: P.Parser (Text, Text)
passportEntry = (,) <$> passportKey <* ":" <*> passportValue
  where
    passportKey = choice ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
    passportValue = Text.pack <$> anySingle `manyTill` lookAhead space1

passportEntryMap :: P.Parser EntryMap
passportEntryMap = Map.fromList <$> passportEntry `sepEndBy1` (char ' ' <|> newline)

passportEntryMaps :: P.Parser [EntryMap]
passportEntryMaps = passportEntryMap `sepBy` space1

-- Entry map values

color :: P.Parser Color
color = Color <$> (char '#' *> count 6 hexDigitChar)

height :: P.Parser Height
height =
  choice
    [ try (InCm <$> P.intInRange (150, 193) <* "cm"),
      try (InInches <$> P.intInRange (59, 76) <* "in")
    ]

eyeColor :: P.Parser EyeColor
eyeColor =
  choice
    [ Amber <$ "amb",
      Blue <$ "blu",
      Brown <$ "brn",
      Gray <$ "gry",
      Green <$ "grn",
      Hazel <$ "hzl",
      Other <$ "oth"
    ]

passportID :: P.Parser String
passportID = count 9 (oneOf ['0' .. '9'])

countryID :: P.Parser String
countryID = some anySingle

input :: P.Parser [Map.Map Text Text]
input = passportEntryMaps
