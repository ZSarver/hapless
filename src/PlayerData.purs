module PlayerData where

import Prelude
import CardData (Hand(..), fireBomb)
import Facing
import XY
import Data.Generic.Rep (class Generic)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Foreign.Class (class Encode)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
  
newtype Player = Player
    { hand :: Hand
    , location :: XY
    , facing :: Facing
    }

derive instance genericPlayer :: Generic Player _
instance showPlayer :: Show Player where show = genericShow
instance encodePlayer :: Encode Player where encode = genericEncode defaultOptions
instance eqPlayer :: Eq Player where eq = genericEq


dummyPlayer = Player {hand: [fireBomb], location: XY {x: 2, y: 1}, facing: East}

dummyPlayer2 = Player {hand: [fireBomb], location: XY {x: 2, y: 1}, facing: North}
