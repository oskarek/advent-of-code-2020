module AdventPrelude.Bool
  ( xor,
  )
where

-- | Logical xor.
xor :: Bool -> Bool -> Bool
xor = (/=)
