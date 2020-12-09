{-# LANGUAGE TemplateHaskell #-}

module Day8.Types where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, execStateT)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lens.Simple (makeLenses)

data Op = Acc | Jmp | Nop deriving (Eq, Show)

data BootCodeInstr = BootCodeInstr Op Int | End deriving (Eq, Show)

data BootCodeProgState = BootCodeProgState {_accumulator :: Int, _pointer :: Int, _prevPointers :: Set.Set Int} deriving (Eq)

makeLenses ''BootCodeProgState

-- | Type of a program that runs navigation instructions.
type BootCodeProg = ReaderT (Seq.Seq BootCodeInstr) (StateT BootCodeProgState (Except String))

-- | Execute the bitmask program from an empty intitial state.
execBootCodeProg :: Seq.Seq BootCodeInstr -> BootCodeProg a -> Either String BootCodeProgState
execBootCodeProg instrs =
  runExcept . flip execStateT (BootCodeProgState 0 0 Set.empty) . flip runReaderT instrs
