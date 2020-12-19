module Day6.Day6 where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Day6.Parser as Parser
import Types.Solution (Solution (..))

sol :: (Set Char -> Set Char -> Set Char) -> Solution
sol combineSets =
  MkSol
    { parse = Parser.input,
      solve = sum . fmap (Set.size . foldr1 combineSets)
    }

part1, part2 :: Solution
part1 = sol Set.union
part2 = sol Set.intersection
