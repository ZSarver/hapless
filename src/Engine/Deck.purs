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
import Data.Array as Arr

import Core.Util
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
defaultDrawSize = 3

-- draws up to max hand size
draw :: forall e. Int -> Engine e Unit
draw n = do
  (GameState g) <- get
  deck <- mightShuffle g
  let hand' = g.hand <> (take (canDraw g) $ drawPile deck)
  let deck' = Deck (drop (canDraw g) $ drawPile deck) (discardPile deck)
  put $ GameState (g {hand = hand', deck = deck'})
  where
    canDraw g = min (maxHandSize - (length g.hand)) n
    mightShuffle g
      | (canDraw g) > (length (drawPile g.deck)) = liftEff $ shuffleDeck g.deck
      | otherwise = pure g.deck

pushToDiscard :: forall e. ShortCard -> Engine e Unit
pushToDiscard c = modify $ liftDeck $ \(Deck draw discard) -> Deck draw (Arr.cons c discard)

discardAt :: forall e. Int -> Engine e Unit
discardAt i = do
  (GameState g) <- get
  case popAt i g.hand  of
    Nothing -> pure unit
    Just (Tuple c cs) -> do
      pushToDiscard c
      modify $ liftHand $ const cs
  
discardN :: forall e. Int -> Engine e Unit
discardN n = do
  (GameState g) <- get
  let h = g.hand
  let discarded = if (length h) < n then [] else (take n h)
  let hand' = if (length h) < n then h else (drop n h)
  let deck' = Deck (drawPile g.deck) ((discardPile g.deck) <> discarded)
  put $ GameState (g {hand = hand', deck = deck'})
