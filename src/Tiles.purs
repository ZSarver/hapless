module Tiles where

import Prelude
import Data.Foldable (foldr)
import Data.StrMap as M

tileSet :: String
tileSet = "combined_tilemap.png"

type TextureCoords = { x :: Int, y :: Int }
data Tile = T String Int Int

tileMap :: M.StrMap TextureCoords
tileMap =  fromTiles 
  [ T "player_down" 4 0
  , T "player_left" 4 1
  , T "player_right" 4 2
  , T "player_up" 4 3
  ]

fromTiles :: Array Tile -> M.StrMap TextureCoords
fromTiles arr = foldr f M.empty arr
  where
    f :: Tile -> M.StrMap TextureCoords -> M.StrMap TextureCoords
    f (T name x y) = M.insert name {x: x, y: y}




