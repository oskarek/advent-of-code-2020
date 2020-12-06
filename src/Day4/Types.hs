module Day4.Types where

import Data.Text (Text)
import Data.Map (Map)

data Height = InCm Int | InInches Int
newtype Color = Color String
data EyeColor = Amber | Blue | Brown | Gray | Green | Hazel | Other
type EntryMap = Map Text Text

data Passport = Passport
  { byr :: Int,
    iyr :: Int,
    eyr :: Int,
    hgt :: Height,
    hcl :: Color,
    ecl :: EyeColor,
    pid :: String,
    cid :: Maybe String
  }