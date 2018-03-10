module Engine.Deck where

import Batteries
import Data.Array(updateAt, reverse, range, (..), zipWith, take, drop)
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

maxHandSize :: Int
maxHandSize = 10

defaultDrawSize :: Int
defaultDrawSize = 10

-- draws up to max hand size
draw :: forall e. Int -> GameState -> Eff (random :: RANDOM | e) GameState
draw n (GameState g) = do
  deck <- mightShuffle
  let hand = take canDraw $ drawPile deck
  let deck' = Deck (drop canDraw $ drawPile deck) (discardPile deck)
  pure $ GameState g
  where
    canDraw = min (maxHandSize - (length g.hand)) defaultDrawSize
    mightShuffle
      | canDraw > (length (drawPile g.deck)) = shuffleDeck g.deck
      | otherwise = pure g.deck
