module Box where

import Prelude
import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (genericEncode, defaultOptions)
import Data.Generic.Rep.Eq (genericEq)
import Data.Newtype (class Newtype)

newtype Box = Box { width :: Int, height :: Int }

derive instance newtypeBox :: Newtype Box _
derive instance genericBox :: Generic Box _
instance showBox :: Show Box where show = genericShow
instance encodeBox :: Encode Box where encode = genericEncode defaultOptions
instance eqBox :: Eq Box where eq = genericEq




