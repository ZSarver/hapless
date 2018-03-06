module XY where

import Prelude
import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (genericEncode, defaultOptions)
import Data.Generic.Rep.Eq (genericEq)
import Data.Newtype (class Newtype)

newtype XY = XY { x :: Int, y :: Int }

derive instance newtypeXY :: Newtype XY _
derive instance genericXY :: Generic XY _
instance showXY :: Show XY where show = genericShow
instance encodeXY :: Encode XY where encode = genericEncode defaultOptions
instance eqXY :: Eq XY where eq = genericEq




