{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Solution (Solution (..), printSolutions) where

import Data.Text (Text)
import Types.Parser (Parser)
import qualified Types.Parser as P

data Solution = forall i o. (Show o, Show o) =>
  MkSol { parse :: Parser i
        , solve :: i -> o }

printSolutionsOutput :: (String, String) -> String
printSolutionsOutput (s1, s2) =
  unlines ["Solution to part 1:", s1, "\nSolution to part 2:", s2]

printSolution :: Solution -> Text -> String
printSolution MkSol { .. } input = either P.printParseError show (solve <$> P.parse parse input)

printSolutions :: (Solution, Solution) -> Text -> String
printSolutions (sol1, sol2) input =
  printSolutionsOutput (printSolution sol1 input, printSolution sol2 input)
