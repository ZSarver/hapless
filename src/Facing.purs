module Facing where

import Prelude
import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (genericEncode, defaultOptions)
import Data.Generic.Rep.Eq (genericEq)

data Facing = North | South | East | West

derive instance genericFacing :: Generic Facing _
instance showFacing :: Show Facing where show = genericShow
instance encodeFacing :: Encode Facing where encode = genericEncode defaultOptions
instance eqFacing :: Eq Facing where eq = genericEq



