module Core.GameState where

import Batteries

import Core.Box
import Core.Player (Player(..), dummyPlayer)
import Content.Enemies (Enemy(..), dummyEnemy, dummyEnemy2)
import Core.Bestiary
import Content.Cards (Hand, ShortCard (..))
import Data.Maybe
import Data.Identity
import Control.Monad.Except.Trans
import Optic.Lens
import Optic.Types
import Content.XY
import Data.Either
import Core.Deck
import Data.Array as Arr
import Content.Deck


newtype GameState = GameState
  { player :: Player
  , hand :: Hand
  , hp :: Int
  , enemies :: Array Enemy
  , bestiary :: Bestiary
  , boundaries :: Box
  , deck :: Deck
  , stairs ∷ XY
  , floor ∷ Int
  }

liftHp :: (Int -> Int) -> GameState -> GameState
liftHp f (GameState g) = GameState g{ hp = f g.hp }

liftDeck :: (Deck -> Deck) -> GameState -> GameState
liftDeck f (GameState g) = GameState g{ deck = f g.deck }

liftHand :: (Hand -> Hand) -> GameState -> GameState
liftHand f (GameState g) = GameState g{ hand = f g.hand }

liftPlayer :: (Player -> Player) -> GameState -> GameState
liftPlayer f (GameState g) = GameState g{ player = f g.player }

liftEnemies :: (Array Enemy -> Array Enemy) -> GameState -> GameState
liftEnemies f (GameState g) = GameState g{ enemies = f g.enemies }

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where 
  show = genericShow 
instance eqGameState :: Eq GameState where
  eq = genericEq

serialize :: GameState -> String
serialize g = genericEncodeJSON defaultOptions g

deserialize :: String -> Maybe GameState
deserialize s = case un Identity (runExceptT (genericDecodeJSON defaultOptions s)) of
  Right x -> Just x
  Left _ -> Nothing

removeEnemyAt :: XY -> GameState -> GameState
removeEnemyAt xy = liftEnemies $ Arr.filter (\(Enemy e) -> e.location /= xy)
 
at :: GameState -> XY -> Variant (player :: Player, enemy :: Enemy, empty :: Unit, wall:: Unit)
at (GameState g) xy
  | (un Player g.player).location == xy = inj (SProxy :: SProxy "player") g.player
  | not $ inBounds xy g.boundaries = inj (SProxy :: SProxy "wall") unit
  | otherwise = 
      case find (\(Enemy e) -> e.location == xy) g.enemies of
        (Just e) -> inj (SProxy :: SProxy "enemy") e
        Nothing -> inj (SProxy :: SProxy "empty") unit

inBounds :: XY -> Box -> Boolean
inBounds (XY l) (Box b) = (b.xmin <= l.x) && (b.xmax >= l.x) && (b.ymin <= l.y) && (b.ymax >= l.y)

movePlayerTo :: GameState -> XY -> GameState
movePlayerTo gs@(GameState g) xy@(XY l) = if legal
  then GameState g{ player = p' }
  else gs
  where
    p' = over Player ( _{ location = xy } ) g.player
    legal = inBounds xy g.boundaries

getLocation :: GameState -> XY
getLocation (GameState g) = p.location
  where
    (Player p) = g.player

locationLens :: Lens' GameState XY
locationLens = lens (getLocation) (movePlayerTo)

dummyGameState :: GameState
dummyGameState = GameState 
  { player: dummyPlayer
  , hand: [Advance, TurnLeft, Advance, TurnRight, Advance]
  , hp: 9
  , enemies: [dummyEnemy, dummyEnemy2]
  , bestiary: dummyBestiary 
  , boundaries: Box {xmin: 1, xmax: 6, ymin: 1, ymax: 6}
  , deck: Deck startingDeck ([] :: Array ShortCard)
  , stairs: XY {x: 3, y: 3}
  , floor: 1
  }

