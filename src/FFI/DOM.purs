module FFI.DOM where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

foreign import data DOM :: Effect

foreign import data DebugBox :: Type

foreign import debugBox :: forall e. Eff (dom :: DOM | e) DebugBox

foreign import toDebug :: forall e. String -> DebugBox -> Eff (dom :: DOM | e) Unit

foreign import fromDebug :: forall e. DebugBox -> Eff (dom :: DOM | e) String

foreign import putCardText :: forall e. Int -> String -> Eff (dom :: DOM | e) Unit

foreign import clearCardText :: forall e. Int -> Eff (dom :: DOM | e) Unit
