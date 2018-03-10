module Core.GameState where

import Batteries

import Core.Box
import Core.Player (Player(..), dummyPlayer)
import Content.Enemies (Enemy(..), dummyEnemy)
import Core.Bestiary
import Content.Cards (Hand, ShortCard (..))
import Data.Maybe
import Data.Identity
import Control.Monad.Except.Trans
import Optic.Lens
import Optic.Types
import Content.XY
import Data.Either


newtype GameState = GameState
  { player :: Player
  , hand :: Hand
  , hp :: Int
  , enemies :: Array Enemy
  , bestiary :: Bestiary
  , boundaries :: Box
  }

liftHp :: (Int -> Int) -> GameState -> GameState
liftHp f (GameState g) = GameState g{ hp = f g.hp }

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

at :: GameState -> XY -> Variant (player :: Player, enemy :: Enemy, empty :: Unit)
at (GameState g) xy = 
  if (un Player g.player).location == xy 
    then inj (SProxy :: SProxy "player") g.player
    else 
      case find (\(Enemy e) -> e.location == xy) g.enemies of
        (Just e) -> inj (SProxy :: SProxy "enemy") e
        Nothing -> inj (SProxy :: SProxy "empty") unit

setLocation :: GameState -> XY -> GameState
setLocation (GameState g) l = GameState g'
  where
    (Player p) = g.player
    p' = p {location = l}
    g' = g {player = Player p'}

getLocation :: GameState -> XY
getLocation (GameState g) = p.location
  where
    (Player p) = g.player

locationLens :: Lens' GameState XY
locationLens = lens (getLocation) (setLocation)

dummyGameState :: GameState
dummyGameState = GameState 
  { player: dummyPlayer
  , hand: [FireBomb, Advance, TurnLeft, Advance, TurnRight, Advance, Advance, Advance, Advance, Advance]
  , hp: 10
  , enemies: [dummyEnemy]
  , bestiary: dummyBestiary 
  , boundaries: Box {xmin: 1, xmax: 6, ymin: 1, ymax: 6}
  }
