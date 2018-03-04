module RotFFI where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Aff
import Control.Monad.Aff.Compat
import Control.Monad.Eff.Class (liftEff)

foreign import data ROT :: Effect
foreign import data Display :: Type

newtype DisplayOptions = DisplayOptions
  { width  :: Number
  , height :: Number
  }

foreign import _init :: forall e. DisplayOptions -> Eff (rot :: ROT | e) Display
init :: forall e. DisplayOptions -> Aff (rot :: ROT | e) Display
init = liftEff <<< _init

foreign import _clear :: forall e. Display -> Eff (rot :: ROT | e) Unit
clear :: forall e. Display -> Aff (rot :: ROT | e) Unit
clear = liftEff <<< _clear

foreign import data Keyboard :: Type
foreign import _initKeyboardHandler :: forall e. Eff (rot :: ROT | e) Keyboard
initKeyboardHandler :: forall e. Aff (rot :: ROT | e) Keyboard
initKeyboardHandler = liftEff _initKeyboardHandler

newtype Key = Key { keyCode :: String }
foreign import _getKey :: forall e. Keyboard -> EffFnAff (rot :: ROT | e) Key
getKey :: forall e. Keyboard -> Aff (rot :: ROT | e) Key
getKey = fromEffFnAff <<< _getKey







