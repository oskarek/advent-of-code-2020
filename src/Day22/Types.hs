{-# LANGUAGE TemplateHaskell #-}

module Day22.Types where

import Control.Monad.State (State, execState)
import Data.IntMap.NonEmpty (NEIntMap)
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Set (Set, empty)
import Lens.Simple (makeLenses, view)

type Deck = NESeq Int

type PlayerDecks = NEIntMap Deck

data CombatGameState = CombatGameState
  { _playerDecks :: PlayerDecks,
    _prevDecks :: Set PlayerDecks
  }

makeLenses ''CombatGameState

type CombatGame = State CombatGameState ()

-- | Execute the Combat game from an intitial state.
execCombatGame :: CombatGame -> PlayerDecks -> PlayerDecks
execCombatGame game = view playerDecks . execState game . flip CombatGameState empty
