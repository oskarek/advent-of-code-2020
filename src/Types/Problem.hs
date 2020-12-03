{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Problem (Problem (..), printSolution) where

import Control.Arrow (Arrow ((&&&)))
import Data.Text (Text)
import Types.Parser (Parser, parse, printParseError)
data Problem = forall a b c. (Show b, Show c) =>
  Problem { parser :: Parser a
          , solvePartOne :: a -> b
          , solvePartTwo :: a -> c }

printSolutions :: (Show a, Show b) => (a, b) -> String
printSolutions (s1, s2) =
  unlines ["Solution to part 1:", show s1, "\nSolution to part 2:", show s2]

printSolution :: Problem -> Text -> String
printSolution Problem {..} input =
  let parseRes = (solvePartOne &&& solvePartTwo) <$> parse parser input
   in either printParseError printSolutions parseRes