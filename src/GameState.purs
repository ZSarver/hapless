module GameState where

import Batteries

import Box
import PlayerData (Player, dummyPlayer)
import EnemyData (Enemy, dummyEnemy)

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


dummyGameState :: GameState
dummyGameState = GameState { player: dummyPlayer, enemies: [dummyEnemy], boundaries: Box {width: 6, height: 6} }
