module GameState where

import PlayerData (Player, dummyPlayer)
import EnemyData (Enemy, dummyEnemy)
import Data.Generic (class Generic)

type GameState =
  { player :: Player
  , enemies :: Array Enemy
  }

dummyGameState :: GameState
dummyGameState = { player: dummyPlayer, enemies: [dummyEnemy] }
