{-# LANGUAGE OverloadedStrings #-}

module Day22.Parser where

import Control.Applicative.Combinators.NonEmpty (sepBy1, sepEndBy1)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.Sequence.NonEmpty as NESeq
import Day22.Types (Deck, PlayerDecks)
import Text.Megaparsec.Char (newline)
import qualified Types.Parser as P

playerDeck :: P.Parser (Int, Deck)
playerDeck = do
  playerId <- "Player " *> P.int <* ":" <* newline
  deck <- NESeq.fromList <$> P.int `sepEndBy1` newline
  return (playerId, deck)

playerDecks :: P.Parser PlayerDecks
playerDecks = NEIntMap.fromList <$> playerDeck `sepBy1` newline