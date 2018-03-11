module Batteries 
  ( module Prelude
  , module Data.Generic.Rep
  , module Data.Generic.Rep.Show
  , module Data.Generic.Rep.Eq
  , module Data.Generic.Rep.Ord
  , module Data.Foreign.Class
  , module Data.Foreign.Generic
  , module Data.Newtype
  , module Data.Foldable
  , module Data.Traversable
  , module Data.Array
  , module Data.Ord
  , module Data.Maybe
  , module Data.Tuple
  , module Data.Variant
  , module Control.Monad.State.Class
  , module Control.Monad.State.Trans
  , module Control.Monad.Writer.Class
  , defaultOptions
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (genericEncode, genericDecode, genericEncodeJSON, genericDecodeJSON)
import Data.Foreign.Generic as G
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Newtype (class Newtype, un, under, over)
import Data.Foldable
import Data.Traversable
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Writer.Class (tell)
import Data.Maybe
import Data.Tuple
import Data.Ord
import Data.Variant (Variant(..), inj, SProxy(..), onMatch, match)
import Data.Array (head, (!!))


defaultOptions = G.defaultOptions { unwrapSingleConstructors = true }

