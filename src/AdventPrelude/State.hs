module AdventPrelude.State where

import Control.Monad.State

-- | Run a state computation until two consecutive states match the predicate.
untilStable :: MonadState s m => (s -> s -> Bool) -> m a -> m a
untilStable p = mfix $ \f st -> do
  before <- get
  a <- st
  after <- get
  if p before after then pure a else f

-- | Run a state computation until the state matches the predicate.
untilS :: MonadState s m => (s -> Bool) -> m a -> m a
untilS p = mfix $ \f st -> do
  a <- st
  s <- get
  if p s then pure a else f
