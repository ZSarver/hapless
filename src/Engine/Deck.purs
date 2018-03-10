module Engine.Deck where

import Batteries
import Data.Array(updateAt, reverse, range, (..), zipWith)
import Control.Bind (join)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random
import Data.Traversable (sequence)
import Data.Array.Shuffle
import Data.Tuple(Tuple(..))

import Core.Deck
import Core.GameState
import Content.Cards

shuffleDeck :: forall e. Deck -> Eff (random :: RANDOM | e) Deck
shuffleDeck d = do
    newdrawpile <- shuffle $ (drawPile d) <> (discardPile d)
    pure $ Deck newdrawpile []