module Main where

import Prelude
import Control.Monad.Aff.Console(CONSOLE, log)
import Control.Monad.Eff (Eff)
import RotFFI
import Control.Monad.Aff
import Control.Monad.Rec.Class(forever)

import Control.Monad.Eff.Class (liftEff)

main :: forall e. Eff ( console :: CONSOLE, rot :: ROT | e) Unit
main = launchAff_ $ do
  log "Hello sailor!"
  let opts = DisplayOptions { width: 8.0, height: 8.0 }
  display <- init opts
  keyboard <- initKeyboardHandler
  forever $ do
    key <- getKey keyboard
    logKey key

logKey :: forall e. Key -> Aff (console :: CONSOLE | e) Unit
logKey (Key k) = log (k.keyCode)



