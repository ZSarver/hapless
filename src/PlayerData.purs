module PlayerData where

import Prelude
import CardData (Hand(..), attack)
import Data.Generic (class Generic, gEq)
import Facing
  
type Player = 
    { hand :: Hand
    , location :: {x :: Int, y :: Int}
    , facing :: Facing
    }


dummyPlayer = {hand: [attack], location: {x: 2, y: 1}, facing: East}

dummyPlayer2 = {hand: [attack], location: {x: 2, y: 1}, facing: North}
