module Core.Box where

import Batteries

newtype Box = Box 
  { xmin :: Int
  , xmax :: Int 
  , ymin :: Int
  , ymax :: Int
  }

derive instance newtypeBox :: Newtype Box _
derive instance genericBox :: Generic Box _
instance showBox :: Show Box where show = genericShow
instance encodeBox :: Encode Box where encode = genericEncode defaultOptions
instance decodeBox :: Decode Box where decode = genericDecode defaultOptions
instance eqBox :: Eq Box where eq = genericEq




