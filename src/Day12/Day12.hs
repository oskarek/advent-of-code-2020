module Day12.Day12 where

import Control.Monad.Reader (ask)
import qualified Day12.Parser as Parser
import Day12.Types (MoveType (..), NavInstr (..), NavProg, NavProgState, execNavProg, pos, velocity)
import Lens.Simple (use, view, (%=), (+=))
import Linear (V2 (..), (^*))
import Types.Problem (Problem (..))

-- | Rotate the vector a number of degrees, evenly divided by 90.
rotate :: Int -> V2 Int -> V2 Int
rotate degrees (V2 x y) =
  case (degrees `div` 90) `mod` 4 of
    0 -> V2 x y
    1 -> V2 y (-x)
    2 -> V2 (-x) (-y)
    3 -> V2 (-y) x

-- | Program that runs a single navigation instruction when executed.
instrProg :: NavInstr -> NavProg
instrProg navInstr = case navInstr of
  Move vect -> do
    moveType <- ask
    case moveType of
      Ship -> pos += vect
      Waypoint -> velocity += vect
  Rotate degrees ->
    velocity %= rotate degrees
  Forward dist -> do
    v <- use velocity
    pos += v ^* dist

-- | Program that runs through a series of navigation instructions, when executed.
prog :: [NavInstr] -> NavProg
prog = mapM_ instrProg

-- | Get the Manhattan distance between the position in the state, and the start
-- position (0,0).
posDist :: NavProgState -> Int
posDist = sum . fmap abs . view pos

problem :: Problem
problem =
  Problem
    { parser = Parser.navInstrs,
      solvePartOne = posDist . execNavProg (V2 1 0) Ship . prog,
      solvePartTwo = posDist . execNavProg (V2 10 1) Waypoint . prog
    }
