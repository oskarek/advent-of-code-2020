module AdventPrelude.Function where

import Data.Function (fix)

-- | Apply the function to the value repeatedly until further application has no
-- effect.
idempotently :: Eq a => (a -> a) -> a -> a
idempotently = fix $ \i f a ->
  let a' = f a
   in if a' == a then a else i f a'

-- | Apply the function to the value n times.
applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ a = a
applyNTimes n f a = applyNTimes (n - 1) f (f a)