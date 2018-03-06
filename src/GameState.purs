module GameState where

import Prelude
import Box
import PlayerData (Player, dummyPlayer)
import EnemyData (Enemy, dummyEnemy)
import Data.Generic.Rep (class Generic)
import Data.Foreign.Generic (defaultOptions, genericEncode, genericDecodeJSON, genericEncodeJSON)
import Data.Foreign.Class (class Encode)
import Data.Generic.Rep.Show (genericShow)


newtype GameState = GameState
  { player :: Player
  , enemies :: Array Enemy
  , boundaries :: Box
  }

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where 
  show = genericShow

to :: GameState -> String
to g = genericEncodeJSON defaultOptions g

dummyGameState :: GameState
dummyGameState = GameState { player: dummyPlayer, enemies: [dummyEnemy], boundaries: Box {width: 6, height: 6} }
