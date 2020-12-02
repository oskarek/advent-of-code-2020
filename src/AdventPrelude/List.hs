module AdventPrelude.List
  ( subsetsOfSize,
    occurrences,
  )
where

-- | Get all subsets of a particular size, treating the list as a multiset.
subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize 0 _ = [[]]
subsetsOfSize _ [] = []
subsetsOfSize n (x : xs) =
  ((x :) <$> subsetsOfSize (n - 1) xs) ++ subsetsOfSize n xs

-- | Get the number of occurrences of a specific element in a list.
occurrences :: Eq a => a -> [a] -> Int
occurrences element = length . filter (== element)
