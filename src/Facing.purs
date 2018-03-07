module Facing where

import Prelude
import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (genericEncode, genericDecode, defaultOptions)
import Data.Generic.Rep.Eq (genericEq)

data Facing = North | South | East | West

rotate :: Int -> Facing -> Facing
rotate n f = fromInt $ ((toInt f) + n) `mod` 4
  where
    toInt South = 0
    toInt West = 1
    toInt North = 2
    toInt East = 3
    fromInt 0 = South
    fromInt 1 = West
    fromInt 2 = North
    fromInt _ = East

left :: Facing -> Facing
left = rotate 1

right :: Facing -> Facing
right = rotate 3

derive instance genericFacing :: Generic Facing _
instance showFacing :: Show Facing where show = genericShow
instance encodeFacing :: Encode Facing where encode = genericEncode defaultOptions
instance decodeFacing :: Decode Facing where decode = genericDecode defaultOptions
instance eqFacing :: Eq Facing where eq = genericEq
