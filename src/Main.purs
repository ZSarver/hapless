module Main where

import Batteries

import Core
import Engine.Deck

import Control.Monad.Aff.Console(CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random
import FFI.Rot (DisplayOptions(..), Key(..), ROT, getKey, initrotjs)
import FFI.DOM (DOM, DebugBox, debugBox, fromDebug, combatLog)
import Control.Monad.Aff(Aff, launchAff_)
import Control.Monad.Rec.Class(forever)
import Control.Monad.Eff.Class (liftEff)
import Content.Tiles (tileSet, tileMap)
import Control.Monad.State.Trans (get, modify, runStateT)
import Control.Monad.Aff.Class (liftAff)
import Data.Array((!!))
import Engine.Cards(play, handleCardEffect)
import Content.Cards(card, ShortCard(..))
import Engine.Enemies (advanceEnemies)
import Engine.Engine
import Engine.Floor
import Debug.Trace (traceAny)

main :: forall e. Eff ( console :: CONSOLE, rot :: ROT, dom :: DOM, random :: RANDOM | e) Unit
main = launchAff_ $ do
  let opts = DisplayOptions { width: 8
                            , height: 8
                            , tileSet: tileSet
                            , tileMap: tileMap
                            , tileSize: 16
                            }
  rotjs <- initrotjs opts
  debug <- liftEff debugBox
  flip execEngine dummyGameState $ do
    genFloor 1
    forever $ do
      gameState <- get
      liftAff $ render (traceAny gameState $ \_ -> gameState) rotjs
      key <- liftAff $ getKey rotjs
      action <- liftAff $ handleKey key debug
      case action of
        Nothing -> pure unit
        Just e -> e

execEngine :: forall e a. Engine e a -> GameState -> Aff (dom :: DOM, random :: RANDOM | e) a
execEngine (Engine e) gs = evalStateT e gs
 

logKey :: forall e. Key -> Aff (console :: CONSOLE | e) Unit
logKey (Key k) = log (show (k.keyCode))

withEngineResponse :: forall e. Engine e Boolean -> Engine e Unit
withEngineResponse action = do
  (GameState g) ← get
  let (Player p) = g.player
  turnConsumed <- action
  let ableToPlay = turnConsumed && g.hp > 0
  let dead = g.hp <= 0
  when ableToPlay $ do
    advanceFloor (g.floor + 1)
    advanceEnemies
    draw 3
  when (dead) $ tellLn "You're dead."

pass :: forall e. Engine e Unit
pass = withEngineResponse $ pure true

handleKey :: forall e. Key -> DebugBox -> Aff (console :: CONSOLE, dom :: DOM, random :: RANDOM | e) (Maybe (Engine (console :: CONSOLE | e) Unit))
handleKey (Key key) debug = do
  action
  where
    k :: Int
    k = key.keyCode
    action 
      | k == debugLoadState = do
        gs <- liftEff $ map deserialize $ fromDebug debug
        log "loading"
        case gs of 
          Just state -> pure (Just (put state))
          Nothing -> do
            log "load failed"
            pure Nothing
      | k >= 49 && k <= 57 = do
        pure $ Just $ withEngineResponse $ play (k - 49)
      | k == zero = do
        pure $ Just $ withEngineResponse $ play 9
      | k == space = pure $ Just $ pass
      | otherwise = do
          log (show k)
          pure Nothing
    
-- backtick `
debugLoadState :: Int
debugLoadState = 192

space :: Int
space = 32

-- 0
zero :: Int
zero = 48

-- 1
one :: Int
one = 49

-- 2
two :: Int
two = 50

-- 3
three :: Int
three = 51

-- 4
four :: Int
four = 52

-- 5
five :: Int
five = 53
