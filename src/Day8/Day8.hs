{-# LANGUAGE FlexibleContexts #-}

module Day8.Day8 where

import AdventPrelude (untilStable)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, unless)
import qualified Data.Foldable as Fold
import Data.List.HT (splitEverywhere)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq
import Data.Set (insert, member)
import qualified Day8.Parser as Parser
import Day8.Types
import Lens.Simple (use, view, (%=), (+=), (^.))
import Types.Solution (Solution (..))

-- | Program that executes a number of boot code instructions, until it loops or
-- terminates successfully.
prog :: BootCodeProg ()
prog = untilStable (==) $ do
  instrs <- ask
  pntr <- use pointer
  prevPntrs <- use prevPointers
  case instrs Seq.!? pntr of
    Nothing -> throwError ("memory access out of bounds: " ++ show pntr)
    Just instr -> unless (pntr `member` prevPntrs) $ do
      prevPointers %= insert pntr
      case instr of
        End -> return ()
        BootCodeInstr op val -> case op of
          Nop -> pointer += 1
          Acc -> do
            accumulator += val
            pointer += 1
          Jmp -> pointer += val

-- | Generate a list of alterntive instruction sequences, each with exactly one
-- Jmp and Nop instr switched.
generatePossibleFixes :: Seq BootCodeInstr -> [Seq BootCodeInstr]
generatePossibleFixes = mapMaybe f . splitEverywhere . Fold.toList
  where
    f (pre, instr, post) =
      let newInstr = case instr of
            BootCodeInstr Jmp v -> Just (BootCodeInstr Nop v)
            BootCodeInstr Nop v -> Just (BootCodeInstr Jmp v)
            _ -> Nothing
       in Seq.fromList . (pre ++) . (: post) <$> newInstr

-- | Try running the various versions of the instructions sequence, until one
-- terminates successfully.
tryOptions :: [Seq BootCodeInstr] -> Either String BootCodeProgState
tryOptions [] = Left "No option terminated successfully"
tryOptions (instrs : rest) =
  case execBootCodeProg instrs prog of
    Right endState | instrs !? (endState ^. pointer) == Just End -> Right endState
    _ -> tryOptions rest

part1 :: Solution
part1 =
  MkSol
    { parse = Parser.instrs,
      solve = fmap (view accumulator) . flip execBootCodeProg prog
    }

part2 :: Solution
part2 =
  MkSol
    { parse = Parser.instrs,
      solve = fmap (view accumulator) . tryOptions . generatePossibleFixes
    }