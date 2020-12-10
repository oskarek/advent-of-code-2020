module Main (main) where

import Data.Map (Map, (!?))
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import qualified Day1.Day1 as Day1
import qualified Day2.Day2 as Day2
import qualified Day3.Day3 as Day3
import qualified Day4.Day4 as Day4
import qualified Day5.Day5 as Day5
import qualified Day6.Day6 as Day6
import qualified Day7.Day7 as Day7
import qualified Day8.Day8 as Day8
import qualified Day9.Day9 as Day9
import Options.Applicative
import Types.Problem (Problem, printSolution)

problems :: Map Int Problem
problems =
  M.fromList
    [ (1, Day1.problem),
      (2, Day2.problem),
      (3, Day3.problem),
      (4, Day4.problem),
      (5, Day5.problem),
      (6, Day6.problem),
      (7, Day7.problem),
      (8, Day8.problem),
      (9, Day9.problem)
    ]

main :: IO ()
main = do
  day <- execParser opts
  case problems !? day of
    Nothing -> putStrLn $ "Day " ++ show day ++ " problem not solved yet!"
    Just problem -> do
      input <- TIO.readFile ("input/day" ++ show day ++ ".txt")
      putStrLn $ printSolution problem input
  where
    infoMod =
      fullDesc
        <> progDesc "Solve the puzzles for day DAY"
        <> header "Advent of Code 2020 solutions"
    opts =
      info (dayArg <**> helper) infoMod
    dayArg =
      argument auto (help "The problem to solve (day of the month)" <> metavar "DAY")