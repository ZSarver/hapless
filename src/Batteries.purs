module Batteries 
  ( module Prelude
  , module Data.Generic.Rep
  , module Data.Generic.Rep.Show
  , module Data.Foreign.Class
  , module Data.Foreign.Generic
  , module Data.Generic.Rep.Eq
  , module Data.Newtype
  , defaultOptions
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (genericEncode, genericDecode, genericEncodeJSON, genericDecodeJSON)
import Data.Foreign.Generic as G
import Data.Generic.Rep.Eq (genericEq)
import Data.Newtype (class Newtype, un)


defaultOptions = G.defaultOptions { unwrapSingleConstructors = true }
