module Day1.Day1 where

import AdventPrelude (subsetsOfSize)
import Data.List (find)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P
import Types.Solution (Solution (..))

sol :: Int -> Solution
sol subsetsSize =
  MkSol
    { parse = P.int `sepEndBy` newline,
      solve = fmap product . find ((== 2020) . sum) . subsetsOfSize subsetsSize
    }

part1, part2 :: Solution
part1 = sol 2
part2 = sol 3
