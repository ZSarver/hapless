module Coordinate where

import Prelude
import Data.Array (zipWith, replicate, length, (..))
import Math (ceil)
import Data.Int (floor, toNumber)
import Data.Generic (class Generic, gEq)

newtype Coordinate = Coordinate {x :: Int, y :: Int}

derive instance genericCoordinate :: Generic Coordinate
instance eqCoordinate :: Eq Coordinate where
    eq = gEq

fst :: Coordinate -> Int
fst (Coordinate c) = c.x

snd :: Coordinate -> Int
snd (Coordinate c) = c.y

rectangle :: Int -> Int -> Array Coordinate
rectangle = rectangleAt $ Coordinate {x: 0, y: 0}

rectangleAt :: Coordinate -> Int -> Int -> Array Coordinate
rectangleAt (Coordinate c) w h = do
  xc <- xcoords
  yc <- ycoords
  pure $ Coordinate {x: xc, y: yc}
    where 
        xoffsets = map ((-) (ceil ((toNumber w) / 2.0))) (map toNumber (1..w))
        xcoords = zipWith (+) (map floor xoffsets) (replicate (length xoffsets) c.x)
        yoffsets = map ((-) (ceil ((toNumber h) / 2.0))) (map toNumber (1..h))
        ycoords = zipWith (+) (map floor yoffsets) (replicate (length yoffsets) c.y)

dummyCoordinate :: Coordinate
dummyCoordinate = Coordinate {x: 0, y: 0}