module PlayerData where

import Prelude
import CardData (Hand(..), attack)
  
data Facing = North | South | East | West

instance facingEq :: Eq Facing where
    eq North North = true
    eq South South = true
    eq East East = true
    eq West West = true
    eq _ _ = false

newtype Player = Player 
    { hand :: Hand
    , location :: {x :: Int, y :: Int}
    , facing :: Facing
    }

dummyPlayer = Player {hand: [attack], location: {x: 2, y: 1}, facing: East}

dummyPlayer2 = Player {hand: [attack], location: {x: 2, y: 1}, facing: North}