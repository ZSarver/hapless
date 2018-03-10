module Core.Enemies where

import Batteries

import Data.Map (lookup)
import Core.Bestiary
import Content.XY
import Content.Enemies
import Core.Geometry
import Core.Player
import Partial (crashWith)

data Action
  = Forward
  | Left
  | Right
  | Pass


turnToward :: XY -> Action
turnToward (XY xy)
  | xy.x >= 0 = Left
  | otherwise = Right


-- If it can move forward without increasing distance, it does so
-- turns quickly
-- confused if player is directly behind
move :: MoveBehavior -> XY -> Array Action
move Steadfast (XY xy) 
  | xy.y > 0  = [Forward]
  | xy.x > 0  = [Left, Forward]
  | xy.x < 0  = [Right, Forward]
  | otherwise = []

-- Always tries to reduce the greater of horizontal or vertical distance
-- turns slowly
move Waffly target@(XY xy) 
  | xy.y < (abs xy.x) = [turnToward target]
  | xy.y > 0          = [Forward]
  | otherwise         = [turnToward target]

-- Does not turn left
-- turns quickly
move Righty (XY xy)
  | xy.y > 0  = [Forward]
  | xy.x < 0  = [Right, Forward]
  | otherwise = [Right, Right]


enemyAction :: Partial => Bestiary -> Enemy -> Player -> Array Action
enemyAction (Bestiary b) (Enemy e) (Player p) = move behavior (absoluteToLocal e p.location)
  where
    behavior = case (lookup e.species b) of
                 Just (Tuple atk mv) -> mv
                 Nothing -> crashWith $ "Species " <> show e.species <> " missing from bestiary."

