module Day6.Types where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

type YesAnswers = Set.Set Char

type GroupYesAnswers = NE.NonEmpty YesAnswers
