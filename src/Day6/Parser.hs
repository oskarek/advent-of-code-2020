module Day6.Parser where

import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import qualified Data.Set as Set
import Day6.Types (GroupYesAnswers, YesAnswers)
import Text.Megaparsec (oneOf, sepBy, some)
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P

yesAnswers :: P.Parser YesAnswers
yesAnswers = Set.fromList <$> some (oneOf ['a' .. 'z'])

groupYesAnswers :: P.Parser GroupYesAnswers
groupYesAnswers = yesAnswers `sepEndBy1` newline

input :: P.Parser [GroupYesAnswers]
input = groupYesAnswers `sepBy` newline
