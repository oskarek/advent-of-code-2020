module Day22.Day22 where

import AdventPrelude (untilS)
import Control.Monad (guard)
import Control.Monad.State (get)
import Data.Foldable (maximumBy, toList)
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq (..), (><|))
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as Set
import qualified Day22.Parser as Parser
import Day22.Types
import Lens.Simple (use, (%=), (.=))
import Types.Solution (Solution (..))

-- | Check if the current state is a game over state.
gameOver :: CombatGameState -> Bool
gameOver (CombatGameState decks _) = NEIntMap.size decks == 1

-- | Move cards to the winner of the round.
moveCardsToWinner :: Int -> PlayerDecks -> PlayerDecks
moveCardsToWinner winner decks =
  let winnerTop :<|| winnerDeck = decks NEIntMap.! winner
      otherDecks = NEIntMap.filterWithKey (\p _ -> p /= winner) decks
      newOtherDecks = IntMap.mapMaybe (NESeq.nonEmptySeq . NESeq.tail) otherDecks
      otherTops = NESeq.head . snd <$> IntMap.toList otherDecks
      newWinnerDeck = winnerDeck ><| (winnerTop :<|| Seq.fromList otherTops)
   in NEIntMap.insertMap winner newWinnerDeck newOtherDecks

-- | Get the player with the highest-valued top card in their deck.
highestTopCard :: PlayerDecks -> Int
highestTopCard decks =
  fst $ maximumBy (comparing $ NESeq.head . snd) $ NEIntMap.toList decks

-- | A round of Combat.
round' :: CombatGame
round' = do
  decks <- use playerDecks
  playerDecks %= moveCardsToWinner (highestTopCard decks)

-- | A round of Recursive Combat.
recRound :: CombatGame
recRound = do
  CombatGameState decks prevs <- get
  prevDecks %= Set.insert decks
  if decks `Set.member` prevs
    then playerDecks .= uncurry NEIntMap.singleton (NEIntMap.findMin decks)
    else
      let winner = maybe (highestTopCard decks) (getWinner . execCombatGame recGame) (mkSubGameDecks decks)
       in playerDecks %= moveCardsToWinner winner

-- | Try to extract sub game decks from the current player decks.
mkSubGameDecks :: PlayerDecks -> Maybe PlayerDecks
mkSubGameDecks = mapM (\(n :<|| rest) -> NESeq.nonEmptySeq $ takeExactly n rest)
  where
    takeExactly n seq = Seq.take n seq <* guard (length seq >= n)

-- | Given some player decks, that are assumed to be containing only _one_ deck,
-- return the winning player id together with the winning score.
getWinnerWithScore :: PlayerDecks -> (Int, Int)
getWinnerWithScore decks =
  let ((winner, winningDeck) NE.:| []) = NEIntMap.toList decks
   in (winner, sum (zipWith (*) (reverse $ toList winningDeck) [1 ..]))

-- | Get the winner.
getWinner :: PlayerDecks -> Int
getWinner = fst . getWinnerWithScore

-- | Get the winning score.
getWinnerScore :: PlayerDecks -> Int
getWinnerScore = snd . getWinnerWithScore

-- Game versions

-- | A regular game of Combat.
game :: CombatGame
game = untilS gameOver round'

-- | A game of Recursive Combat.
recGame :: CombatGame
recGame = untilS gameOver recRound

part1 :: Solution
part1 =
  MkSol
    { parse = Parser.playerDecks,
      solve = getWinnerScore . execCombatGame game
    }

part2 :: Solution
part2 =
  MkSol
    { parse = Parser.playerDecks,
      solve = getWinnerScore . execCombatGame recGame
    }
