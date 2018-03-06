module GameState where

import Batteries

import Box
import PlayerData (Player(..), dummyPlayer)
import EnemyData (Enemy, dummyEnemy)
import Optic.Lens
import Optic.Types
import XY

newtype GameState = GameState
  { player :: Player
  , enemies :: Array Enemy
  , boundaries :: Box
  }

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where 
  show = genericShow

serialize :: GameState -> String
serialize g = genericEncodeJSON defaultOptions g

deserialize :: String -> _ GameState
deserialize s = genericDecodeJSON defaultOptions s

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
dummyGameState = GameState { player: dummyPlayer, enemies: [dummyEnemy], boundaries: Box {width: 6, height: 6} }
