module Main where

import Prelude
import Control.Monad.Aff.Console(CONSOLE, log)
import Control.Monad.Eff (Eff)
import RotFFI
import Control.Monad.Aff(Aff, launchAff_)
import Control.Monad.Rec.Class(forever)
import Tiles (tileSet, tileMap)
import Render
import GameState (dummyGameState)



main :: forall e. Eff ( console :: CONSOLE, rot :: ROT | e) Unit
main = launchAff_ $ do
  log "Hello sailor!"
  let opts = DisplayOptions { width: 8
                            , height: 8
                            , tileSet: tileSet
                            , tileMap: tileMap
                            , tileSize: 16
                            }
  rotjs <- initrotjs opts
  forever $ do
    render dummyGameState rotjs
    key <- getKey rotjs
    logKey key

logKey :: forall e. Key -> Aff (console :: CONSOLE | e) Unit
logKey (Key k) = log (k.keyCode)





