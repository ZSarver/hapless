module Render where

import Prelude
import GameState
import PlayerData
import Content.Enemies
import Facing
import XY
import FFI.Rot (ROT, RotInstance, clear, putTile, putTile2)
import Control.Monad.Aff
import Content.Tiles
import Data.Array
import Data.Traversable (sequence_)


placeWalls :: forall e. RotInstance -> Aff (rot :: ROT | e) Unit
placeWalls rotjs = sequence_ $ map render1wall wallspaces
  where 
    render1wall (XY s) = putTile wall s.x s.y rotjs
    wallspaces = concat [top, bottom, left, right]
    top    = map (\x -> XY {y: 0, x: x}) $ range 0 7
    bottom = map (\x -> XY {y: 7, x: x}) $ range 0 7
    left   = map (\y -> XY {x: 0, y: y}) $ range 1 6
    right  = map (\y -> XY {x: 7, y: y}) $ range 1 6

placeFloor :: forall e. RotInstance -> Aff (rot :: ROT | e) Unit
placeFloor rotjs = sequence_ $ map render1floor floorspaces
  where
    render1floor s = putTile floor s.x s.y rotjs
    floorspaces = do
      x <- range 1 6
      y <- range 1 6
      pure {x: x, y: y}

render :: forall e. GameState -> RotInstance -> Aff (rot :: ROT | e) Unit
render (GameState gs) rotjs = do
  clear rotjs
  placeWalls rotjs
  placeFloor rotjs
  renderPlayer gs.player rotjs
  sequence_ $ map (flip renderEnemy rotjs) gs.enemies

renderPlayer :: forall e. Player -> RotInstance -> Aff (rot :: ROT | e) Unit
renderPlayer (Player p) rotjs = putTile2 img floor x y rotjs
  where 
    (XY loc) = p.location
    x = loc.x
    y = loc.y
    img = case p.facing of
            North -> playerUp
            South -> playerDown
            East -> playerRight
            West -> playerLeft

renderEnemy :: forall e. Enemy -> RotInstance -> Aff (rot :: ROT | e) Unit
renderEnemy (Enemy e) rotjs = putTile2 img floor loc.x loc.y rotjs
  where
    (XY loc) = e.location
    img = enemyImage e.species e.facing
    enemyImage Skeleton North = skeletonUp
    enemyImage Skeleton South = skeletonDown
    enemyImage Skeleton East = skeletonLeft
    enemyImage Skeleton West = skeletonRight
    enemyImage Ghost North = ghostUp
    enemyImage Ghost South = ghostDown
    enemyImage Ghost East = ghostLeft
    enemyImage Ghost West = ghostRight
    enemyImage Slime North = slimeUp
    enemyImage Slime South = slimeDown
    enemyImage Slime East = slimeLeft
    enemyImage Slime West = slimeRight
