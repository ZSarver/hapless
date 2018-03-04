module RotFFI where

import Control.Monad.Eff (Eff, kind Effect)

foreign import data ROT :: Effect

foreign import data Display :: Type

newtype DisplayOptions = DisplayOptions
  { width  :: Number
  , height :: Number
  }

foreign import init :: forall e. DisplayOptions -> Eff (rot :: ROT | e) Display


