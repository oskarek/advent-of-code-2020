{-# LANGUAGE TemplateHaskell #-}

module Day12.Types where

import Control.Monad.Reader
import Control.Monad.State
import Lens.Simple
import Linear.V2 (V2 (..))

data NavProgState = ProgState {_pos :: V2 Int, _velocity :: V2 Int}

makeLenses ''NavProgState

data MoveType = Ship | Waypoint

-- | Type of a program that runs navigation instructions.
type NavProg = ReaderT MoveType (State NavProgState) ()

-- | Execute the navigation program, providing a startVelocity and a moveType.
-- The moveType determines which object moves by the Move instructions.
execNavProg :: V2 Int -> MoveType -> NavProg -> NavProgState
execNavProg startVelocity moveType prog =
  flip execState (ProgState (V2 0 0) startVelocity) $ runReaderT prog moveType

-- | A navigation instruction.
data NavInstr
  = Move (V2 Int)
  | Rotate Int
  | Forward Int
