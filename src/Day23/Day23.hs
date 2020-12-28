module Day23.Day23 where

import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt)
import qualified Data.List as List
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed.Mutable as VM
import Text.Megaparsec (many)
import Text.Megaparsec.Char (digitChar)
import Types.Solution (Solution (..))

data Circular s = Circular Int Int (VM.MVector s Int)

move :: Circular s -> ST s (Circular s)
move (Circular curr size vec) = do
  a <- VM.read vec curr
  b <- VM.read vec a
  c <- VM.read vec b
  let dest = fromJust $ List.find (`Set.notMember` Set.fromList [a, b, c]) ((`mod` size) . (curr -) <$> [1 ..])
  cSucc <- VM.read vec c
  destSucc <- VM.read vec dest
  sequence_ $ uncurry (VM.write vec) <$> [(curr, cSucc), (c, destSucc), (dest, a)]
  return (Circular cSucc size vec)

moveN :: Int -> Circular s -> ST s (Circular s)
moveN 0 circ = return circ
moveN n circ = move circ >>= moveN (n - 1)

listToCirc :: [Int] -> ST s (Circular s)
listToCirc l = do
  vect <- VM.new (length l)
  mapM_ (uncurry $ VM.write vect) $ zip l (drop 1 l ++ [head l])
  return (Circular (head l) (length l) vect)

circToList :: Circular s -> ST s [Int]
circToList (Circular _ _ vec) = VM.read vec 0 >>= fmap (0 :) . go
  where
    go 0 = return []
    go i = (i :) <$> (VM.read vec i >>= go)

makeNMoves :: Int -> [Int] -> [Int]
makeNMoves n lbls = (+ 1) <$> runST (circToList =<< moveN n =<< listToCirc (subtract 1 <$> lbls))

part1 :: Solution
part1 =
  MkSol
    { parse = many (digitToInt <$> digitChar),
      solve = drop 1 . makeNMoves 100
    }

part2 :: Solution
part2 =
  MkSol
    { parse = many (digitToInt <$> digitChar),
      solve = product . take 2 . drop 1 . makeNMoves 10000000 . (++ [10 .. 1000000])
    }
