module FFI.DOM where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

foreign import data DOM :: Effect

foreign import data DebugBox :: Type

foreign import debugBox :: forall e. Eff (dom :: DOM | e) DebugBox

foreign import fromDebug :: forall e. DebugBox -> Eff (dom :: DOM | e) String

foreign import combatLog :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import setDivContents :: forall a e. a -> String -> Eff (dom :: DOM | e) Unit

putCardText :: forall e. Int -> String -> Eff (dom :: DOM | e) Unit
putCardText id text = setDivContents text ("card " <> show id)

clearCardText :: forall e. Int -> Eff (dom :: DOM | e) Unit
clearCardText id = setDivContents "" ("card " <> show id)

displayFloor :: forall e. Int ->  Eff (dom :: DOM | e) Unit
displayFloor i = setDivContents ("Floor: " <> show i) "floor"

displayHp :: forall e. Int ->  Eff (dom :: DOM | e) Unit
displayHp i = setDivContents ("HP: " <> show i) "hp"

displayDeck :: forall e. Int ->  Eff (dom :: DOM | e) Unit
displayDeck i = setDivContents ("Draw pile: " <> show i) "deck"


displayDiscard :: forall e. Int ->  Eff (dom :: DOM | e) Unit
displayDiscard i = setDivContents ("Discard pile: " <> show i) "discard"

