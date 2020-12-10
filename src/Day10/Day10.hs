{-# LANGUAGE OverloadedLists #-}

module Day10.Day10 where

import AdventPrelude.List (occurrences, zipNeighboursWith)
import AdventPrelude.Num (diff)
import Data.Foldable (foldl')
import Data.IntMap (insert, (!?))
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P
import Types.Problem (Problem (..))

type AdapterRating = Int

solve1 :: [AdapterRating] -> Int
solve1 ratings =
  let diffs = zipNeighboursWith diff (0 : List.sort ratings)
   in occurrences 1 diffs * (occurrences 3 diffs + 1)

numberOfArrangements :: [AdapterRating] -> Int
numberOfArrangements = maximum . foldl' f [(0, 1)] . List.sort
  where
    f ratingMap a =
      let s = sum ((ratingMap !?) `mapMaybe` [a - 1, a - 2, a - 3])
       in insert a s ratingMap

problem :: Problem
problem =
  Problem
    { parser = P.int `sepEndBy` newline,
      solvePartOne = solve1,
      solvePartTwo = numberOfArrangements
    }
