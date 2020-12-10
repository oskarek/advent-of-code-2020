module AdventPrelude.Num where

-- | Get the difference between two numbers.
diff :: Num a => a -> a -> a
diff a b = abs (a - b)
