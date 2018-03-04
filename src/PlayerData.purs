module PlayerData where

import Prelude
import CardData (Hand(..), attack)
  
data Facing = North | South | East | West

derive instance genericFacing :: Generic Facing
instance eqFacing :: Eq Facing where
  eq = geq

newtype Player = Player 
    { hand :: Hand
    , location :: {x :: Int, y :: Int}
    , facing :: Facing
    }

dummyPlayer = Player {hand: [attack], location: {x: 2, y: 1}, facing: East}

dummyPlayer2 = Player {hand: [attack], location: {x: 2, y: 1}, facing: North}