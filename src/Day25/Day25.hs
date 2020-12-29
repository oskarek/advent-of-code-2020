module Day25.Day25 where

import Data.List (elemIndex, unfoldr)
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P
import Types.Solution (Solution (..))

-- | Given a subject number, generate all possible public keys it can create.
pubKeys :: Int -> [Int]
pubKeys subjNum = iterate (\i -> (i * subjNum) `rem` 20201227) 1

-- | Find the loop size that must have been used to create the public key.
findLoopSize :: Int -> Maybe Int
findLoopSize = (`elemIndex` pubKeys 7)

-- | Get the encryption key that the handshake is trying to establish.
findEncryptionKey :: (Int, Int) -> Maybe Int
findEncryptionKey (pubKey1, pubKey2) =
  (pubKeys pubKey2 !!) <$> findLoopSize pubKey1

part1 :: Solution
part1 =
  MkSol
    { parse = (,) <$> (P.int <* newline) <*> (P.int <* newline),
      solve = findEncryptionKey
    }

part2 :: Solution
part2 =
  MkSol
    { parse = return (),
      solve = const "WOHOO!!!"
    }
