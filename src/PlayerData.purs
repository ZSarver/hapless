module PlayerData where

import Batteries
import CardData (Hand(..), fireBomb)
import Facing
import XY
newtype Player = Player
    { hand :: Hand
    , location :: XY
    , facing :: Facing
    }

derive instance newtypePlayer :: Newtype Player _
derive instance genericPlayer :: Generic Player _
instance showPlayer :: Show Player where show = genericShow
instance encodePlayer :: Encode Player where encode = genericEncode defaultOptions
instance decodePlayer :: Decode Player where decode = genericDecode defaultOptions
instance eqPlayer :: Eq Player where eq = genericEq


dummyPlayer = Player {hand: [fireBomb], location: XY {x: 2, y: 1}, facing: East}

dummyPlayer2 = Player {hand: [fireBomb], location: XY {x: 2, y: 1}, facing: North}
