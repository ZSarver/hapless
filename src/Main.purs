module Main where

import Prelude
import Control.Monad.Aff.Console(CONSOLE, log)
import Control.Monad.Eff (Eff)
import RotFFI
import Control.Monad.Aff
import Control.Monad.Rec.Class(forever)
import Control.Monad.Eff.Class (liftEff)
import Tiles (tileSet, tileMap)



main :: forall e. Eff ( console :: CONSOLE, rot :: ROT | e) Unit
main = launchAff_ $ do
  log "Hello sailor!"
  let opts = DisplayOptions { width: 8
                            , height: 8
                            , tileSet: tileSet
                            , tileMap: tileMap
                            , tileSize: 16
                            }
  display <- init opts
  keyboard <- initKeyboardHandler
  putTile "player_down" 0 0 display
  putTile "player_left" 1 1 display
  putTile "player_up" 2 3 display
  forever $ do
    key <- getKey keyboard
    logKey key

logKey :: forall e. Key -> Aff (console :: CONSOLE | e) Unit
logKey (Key k) = log (k.keyCode)



