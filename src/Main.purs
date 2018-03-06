module Main where

import Batteries

import Control.Monad.Aff.Console(CONSOLE, log)
import Control.Monad.Eff (Eff)
import FFI.Rot (DisplayOptions(..), Key(..), ROT, getKey, initrotjs)
import FFI.DOM (DOM, DebugBox, debugBox, fromDebug)
import Control.Monad.Aff(Aff, launchAff_)
import Control.Monad.Rec.Class(forever)
import Control.Monad.Eff.Class (liftEff)
import Content.Tiles (tileSet, tileMap)
import Render (render)
import GameState (GameState, dummyGameState, serialize, deserialize)
import Data.Maybe (Maybe(..))
import Control.Monad.State.Trans (get, modify, runStateT)
import Control.Monad.Aff.Class (liftAff)

main :: forall e. Eff ( console :: CONSOLE, rot :: ROT, dom :: DOM | e) Unit
main = launchAff_ $ do
  log "Hello sailor!"
  let opts = DisplayOptions { width: 8
                            , height: 8
                            , tileSet: tileSet
                            , tileMap: tileMap
                            , tileSize: 16
                            }
  rotjs <- initrotjs opts
  debug <- liftEff debugBox
  flip runStateT dummyGameState $ forever $ do
    gameState <- get
    liftAff $ render gameState rotjs
    key <- liftAff $ getKey rotjs
    action <- liftAff $ handleKey key debug
    case action of
      Nothing -> pure unit
      Just f -> modify f
    liftAff $ log (serialize gameState)

logKey :: forall e. Key -> Aff (console :: CONSOLE | e) Unit
logKey (Key k) = log (show (k.keyCode))

handleKey :: forall e. Key -> DebugBox -> Aff (console :: CONSOLE, dom :: DOM | e) (Maybe (GameState -> GameState))
handleKey (Key key) debug = do
  action
  where
    k :: Int
    k = key.keyCode
    action 
      | k == 192 = do
        gs <- liftEff $ map deserialize $ fromDebug debug
        log "loading"
        case gs of 
          Just state -> pure (Just (\x -> state))
          Nothing -> do
            log "load failed"
            pure Nothing
      | otherwise = do
          log (show k)
          pure Nothing
    
-- backtick `
debugLoadState :: Int
debugLoadState = 192

