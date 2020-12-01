module Misc.ListFunctions (subsetsOfSize) where

-- | Get all subsets of a particular size, treating the list as a multiset.
subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize 0 _ = [[]]
subsetsOfSize _ [] = []
subsetsOfSize n (x : xs) =
  ((x :) <$> subsetsOfSize (n - 1) xs) ++ subsetsOfSize n xs
