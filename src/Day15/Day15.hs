module Day15.Day15 where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (StateT, execStateT, get, put)
import qualified Data.Vector.Unboxed.Mutable as VM
import Text.Megaparsec (sepBy)
import qualified Types.Parser as P
import Types.Solution (Solution (..))

type Game s = StateT Int (ST s) ()

-- | A turn of the game.
turn :: VM.MVector s Int -> Int -> Game s
turn prevsVect prevTurn = do
  prevSpoken <- get
  turnOfPrev <- VM.read prevsVect prevSpoken
  put (if turnOfPrev == -1 then 0 else prevTurn - turnOfPrev)
  VM.write prevsVect prevSpoken prevTurn

-- | A game, with a specified start and end turn.
game :: VM.MVector s Int -> Int -> Int -> Game s
game prevsVect startTurn endTurn =
  mapM_ (turn prevsVect) [startTurn - 1 .. endTurn - 1]

-- | Execute a game until a particular turn, with the given starting numbers.
execGame :: Int -> [Int] -> Int
execGame endTurn startNums = runST $
  flip execStateT (last startNums) $ do
    prevsVect <- VM.replicate endTurn (-1)
    zipWithM_ (VM.write prevsVect) startNums [1 ..]
    game prevsVect (length startNums + 1) endTurn

part1 :: Solution
part1 =
  MkSol
    { parse = P.int `sepBy` P.comma,
      solve = execGame 2020
    }

part2 :: Solution
part2 =
  MkSol
    { parse = P.int `sepBy` P.comma,
      solve = execGame 30000000
    }
