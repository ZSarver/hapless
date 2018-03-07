module PlayerData where

import Batteries
import Content.Cards (Hand(..), ShortCard(..))
import Facing
import XY
newtype Player = Player
    { location :: XY
    , facing :: Facing
    }

derive instance newtypePlayer :: Newtype Player _
derive instance genericPlayer :: Generic Player _
instance showPlayer :: Show Player where show = genericShow
instance encodePlayer :: Encode Player where encode = genericEncode defaultOptions
instance decodePlayer :: Decode Player where decode = genericDecode defaultOptions
instance eqPlayer :: Eq Player where eq = genericEq


dummyPlayer = Player { location: XY {x: 2, y: 1}, facing: East }

dummyPlayer2 = Player { location: XY {x: 2, y: 1}, facing: North }
