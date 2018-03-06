module Content.Tiles where

import Prelude
import Data.Foldable (foldr)
import Data.StrMap as M

tileSet :: String
tileSet = "combined_tilemap.png"

type TextureCoords = { x :: Int, y :: Int }
data Tile = T String Int Int

playerUp :: String
playerUp = "player_up"
playerDown :: String
playerDown = "player_down"
playerLeft :: String
playerLeft = "player_left"
playerRight :: String
playerRight = "player_right"
skeletonUp :: String
skeletonUp = "skeleton_up"
skeletonDown :: String
skeletonDown = "skeleton_down"
skeletonLeft :: String
skeletonLeft = "skeleton_left"
skeletonRight :: String
skeletonRight = "skeleton_right"
slimeUp :: String
slimeUp = "slime_up"
slimeDown :: String
slimeDown = "slime_down"
slimeLeft :: String
slimeLeft = "slime_left"
slimeRight :: String
slimeRight = "slime_right"
ghostUp :: String
ghostUp = "ghost_up"
ghostDown :: String
ghostDown = "ghost_down"
ghostLeft :: String
ghostLeft = "ghost_left"
ghostRight :: String
ghostRight = "ghost_right"
weapon1 :: String
weapon1 = "weapon1"
wall :: String
wall = "wall"
potion1 :: String
potion1 = "potion1"
amulet1 :: String
amulet1 = "amulet1"
boots1 :: String
boots1 = "boots1"
shield1 :: String
shield1 = "shield1"
floor :: String
floor = "floor"

tileMap :: M.StrMap TextureCoords
tileMap =  fromTiles 
  [ T playerDown 4 0
  , T playerLeft 4 1
  , T playerRight 4 2
  , T playerUp 4 3
  , T skeletonDown 10 0
  , T skeletonLeft 10 1
  , T skeletonRight 10 2
  , T skeletonUp 10 3
  , T slimeDown 1 4
  , T slimeLeft 1 5
  , T slimeRight 1 6
  , T slimeUp 1 7
  , T ghostDown 7 4
  , T ghostLeft 7 5
  , T ghostRight 7 6
  , T ghostUp 7 7
  , T weapon1 0 15
  , T wall 19 1
  , T potion1 11 12
  , T amulet1 7 8
  , T boots1 6 18
  , T shield1 6 19
  , T floor 13 9
  ]

fromTiles :: Array Tile -> M.StrMap TextureCoords
fromTiles arr = foldr f M.empty arr
  where
    f :: Tile -> M.StrMap TextureCoords -> M.StrMap TextureCoords
    f (T name x y) = M.insert name {x: x, y: y}




