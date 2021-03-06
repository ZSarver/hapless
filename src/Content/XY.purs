module Content.XY where

import Batteries

import Data.Array (zipWith, replicate, length, (..))
import Math (ceil)
import Data.Int (floor, toNumber)

newtype XY = XY { x :: Int, y :: Int }

derive instance newtypeXY :: Newtype XY _
derive instance genericXY :: Generic XY _
instance showXY :: Show XY where show = genericShow
instance encodeXY :: Encode XY where encode = genericEncode defaultOptions
instance decodeXY :: Decode XY where decode = genericDecode defaultOptions
instance eqXY :: Eq XY where eq = genericEq

instance semigroupXY :: Semigroup XY where
  append (XY xy) (XY ab) = XY { x: xy.x + ab.x, y: xy.y + ab.y }

forward :: XY
forward = XY { x: 0, y: 1 }

back :: XY
back = XY { x:0, y: -1 }

left :: XY
left = XY { x: 1, y: 0 }

right :: XY
right = XY { x: -1, y: 0 }

x :: XY -> Int
x (XY c) = c.x

y :: XY -> Int
y (XY c) = c.y

rectangle :: Int -> Int -> Array XY
rectangle = rectangleAt $ XY {x: 0, y: 0}

rectangleAt :: XY -> Int -> Int -> Array XY
rectangleAt (XY c) w h = do
  xc <- xcoords
  yc <- ycoords
  pure $ XY {x: xc, y: yc}
    where 
        xoffsets = map ((-) (ceil ((toNumber w) / 2.0))) (map toNumber (1..w))
        xcoords = zipWith (+) (map floor xoffsets) (replicate (length xoffsets) c.x)
        yoffsets = map ((-) (ceil ((toNumber h) / 2.0))) (map toNumber (1..h))
        ycoords = zipWith (+) (map floor yoffsets) (replicate (length yoffsets) c.y)

dummyCoordinate :: XY
dummyCoordinate = XY {x: 0, y: 0}
