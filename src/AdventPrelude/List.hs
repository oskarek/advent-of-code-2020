module AdventPrelude.List
  ( subsetsOfSize,
    occurrences,
    takeEvery,
    zipNeighboursWith,
    zipNeighbours,
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

-- | Get every nth element of the list, starting with the first.
takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n (x : xs) = x : takeEvery n (drop (n - 1) xs)

-- | Zip together all neighbours in the list, using the given function.
zipNeighboursWith :: (a -> a -> b) -> [a] -> [b]
zipNeighboursWith f l = zipWith f l (drop 1 l)

-- | Zip together all neighbours in the list.
zipNeighbours :: [a] -> [(a, a)]
zipNeighbours = zipNeighboursWith (,)
