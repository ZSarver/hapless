module GameState where

import PlayerData (Player, dummyPlayer)
import EnemyData (Enemy, dummyEnemy)
import Data.Generic (class Generic)

type GameState =
  { player :: Player
  , enemies :: Array Enemy
  , boundaries :: {width :: Int, height :: Int}
  }

dummyGameState :: GameState
dummyGameState = { player: dummyPlayer, enemies: [dummyEnemy], boundaries: {width: 6, height: 6} }
