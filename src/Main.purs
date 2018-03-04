module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import RotFFI (DisplayOptions(..), init, ROT)
import Cards (attack)

main :: forall e. Eff ( console :: CONSOLE, rot :: ROT | e) Unit
main = do
  log "Hello sailor!"
  let opts = DisplayOptions { width: 8.0, height: 8.0 }
  display <- init opts
  let card = attack
  pure unit
