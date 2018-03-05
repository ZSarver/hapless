module PlayerData where

import Prelude
import CardData (Hand(..), fireBomb)
import Data.Generic (class Generic, gEq)
import Facing
  
newtype Player = Player 
    { hand :: Hand
    , location :: {x :: Int, y :: Int}
    , facing :: Facing
    }

derive instance genericPlayer :: Generic Player

dummyPlayer = Player {hand: [fireBomb], location: {x: 2, y: 1}, facing: East}

dummyPlayer2 = Player {hand: [fireBomb], location: {x: 2, y: 1}, facing: North}
