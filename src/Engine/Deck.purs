module Engine.Deck where

import Batteries
import Data.Array(updateAt, reverse, range, (..), zipWith, take, drop)
import Control.Bind (join)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random
import Data.Traversable (sequence)
import Data.Array.Shuffle
import Data.Tuple(Tuple(..))

import Core.Deck
import Core.GameState
import Content.Cards
import Engine.Engine

shuffleDeck :: forall e. Deck -> Eff (random :: RANDOM | e) Deck
shuffleDeck d = do
  newdrawpile <- shuffle $ (drawPile d) <> (discardPile d)
  pure $ Deck newdrawpile []

maxHandSize :: Int
maxHandSize = 10

defaultDrawSize :: Int
defaultDrawSize = 10

-- draws up to max hand size
draw :: forall e. Int -> Engine e Unit
draw n = do
  (GameState g) <- get
  deck <- mightShuffle g
  let hand' = g.hand <> (take (canDraw g) $ drawPile deck)
  let deck' = Deck (drop (canDraw g) $ drawPile deck) (discardPile deck)
  put $ GameState (g {hand = hand', deck = deck'})
  where
    canDraw g = min (maxHandSize - (length g.hand)) defaultDrawSize
    mightShuffle g
      | (canDraw g) > (length (drawPile g.deck)) = liftEff $ shuffleDeck g.deck
      | otherwise = pure g.deck

discard :: forall e. Int -> Engine e Unit
discard n = do
  (GameState g) <- get
  let h = g.hand
  let discarded = if (length h) < n then [] else (take n h)
  let hand' = if (length h) < n then h else (drop n h)
  let deck' = Deck (drawPile g.deck) ((discardPile g.deck) <> discarded)
  put $ GameState (g {hand = hand', deck = deck'})
