module Main (main) where

import Data.Map (Map, (!?))
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import qualified Day1.Day1 as Day1
import Options.Applicative
import Types.Problem (Problem, printSolution)

problems :: Map Int Problem
problems =
  M.fromList
    [ (1, Day1.problem)
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