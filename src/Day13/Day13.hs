module Day13.Day13 where

import Data.List (find, foldl')
import Data.Maybe (catMaybes, mapMaybe)
import qualified Day13.Parser as Parser
import Types.Problem (Problem (..))

solve1 :: (Int, [Int]) -> Int
solve1 (minT, ids) =
  head $ mapMaybe (\t -> (* (t - minT)) <$> find ((== 0) . rem t) ids) [minT ..]

solve2 :: [Maybe Integer] -> Integer
solve2 ids =
  let pairs = catMaybes $ zipWith (fmap sequence . (,)) [0 ..] ids
   in head $ foldl' (\acc (diff, id) -> continueFirstTwoPattern $ filter (dividesWithOffset diff id) acc) [0 ..] pairs

dividesWithOffset :: Integral a => a -> a -> a -> Bool
dividesWithOffset offset denom num = (num + offset) `mod` denom == 0

-- | Continue the pattern started by the first two elements.
continueFirstTwoPattern :: Enum a => [a] -> [a]
continueFirstTwoPattern (x1 : x2 : _) = [x1, x2 ..]

problem :: Problem
problem =
  Problem
    { parser = Parser.input,
      solvePartOne = solve1 . fmap catMaybes,
      solvePartTwo = solve2 . (fmap . fmap) toInteger . snd
    }
