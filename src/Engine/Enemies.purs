module Engine.Enemies where

import Batteries

import Content.Enemies (Enemy(..), MoveBehavior(..), AttackBehavior(..))
import Core

import Data.Ord
import Data.Map (lookup)
import Partial

data Action
  = Forward
  | Left
  | Right
  | Pass


data Consequence
  = Move XY
  | Noop

interpretAction :: Enemy -> GameState -> Consequence
interpretAction (Enemy e) (GameState g) = Noop



advanceEnemies :: GameState -> GameState
advanceEnemies (GameState g) = GameState g
  where
    enemies = g.enemies
    -- Phase 1: for each enemy, determine if it wants to move, and where to







enemyAction :: Partial => Bestiary -> Enemy -> Player -> Array Action
enemyAction (Bestiary b) (Enemy e) (Player p) = move behavior (absoluteToLocal e p.location)
  where
    behavior = case (lookup e.species b) of
                 Just (Tuple atk mv) -> mv
                 Nothing -> crashWith $ "Species " <> show e.species <> " missing from bestiary."

turnToward :: XY -> Action
turnToward (XY xy)
  | xy.x >= 0 = Left
  | otherwise = Right


-- If it can move forward without increasing distance, it does so
-- turns quickly
-- confused if player is directly behind
move :: MoveBehavior -> XY -> Array Action
move Steadfast (XY xy) 
  | xy.y > 0  = pure Forward
  | xy.x > 0  = [Left, Forward]
  | xy.x < 0  = [Right, Forward]
  | otherwise = pure Pass

-- Always tries to reduce the greater of horizontal or vertical distance
-- turns slowly
move Waffly target@(XY xy) 
  | xy.y < (abs xy.x) = pure $ turnToward target
  | xy.y > 0          = pure Forward
  | otherwise         = pure $ turnToward target

-- Does not turn left
-- turns quickly
move Righty (XY xy)
  | xy.y > 0  = pure Forward
  | xy.x < 0  = [Right, Forward]
  | otherwise = [Right, Right]

move _ _ = pure Pass












