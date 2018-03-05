module GameState where

import PlayerData (Player, dummyPlayer)
import EnemyData (Enemy, dummyEnemy)
import Data.Generic (class Generic)

newtype GameState = GameState
  { player :: Player
  , enemies :: Array Enemy
  , boundaries :: {width :: Int, height :: Int}
  }

derive instance genericGameState :: Generic GameState


dummyGameState :: GameState
dummyGameState = GameState { player: dummyPlayer, enemies: [dummyEnemy], boundaries: {width: 6, height: 6} }
