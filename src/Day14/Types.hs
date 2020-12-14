{-# LANGUAGE TemplateHaskell #-}

module Day14.Types where

import Control.Monad.State (State, execState)
import Data.Map (Map, empty)
import Lens.Simple (makeLenses)

type MemAddress = Int
type Memory = Map MemAddress Int

data MaskBit = Zero | One | X
type Mask = [MaskBit]

data BitMaskInstr = SetMask Mask | WriteToMem MemAddress Int

data BitMaskProgState = BitMaskProgState {_memory :: Memory, _mask :: Mask}

data DecoderChip = Ver1 | Ver2

makeLenses ''BitMaskProgState

-- | Type of a program that runs navigation instructions.
type BitMaskProg = State BitMaskProgState ()

-- | Execute the bitmask program from an empty intitial state.
execBitMaskProg :: BitMaskProg -> BitMaskProgState
execBitMaskProg = flip execState (BitMaskProgState empty [])
