module Core.Geometry
 ( facingAbsoluteToLocal
 , facingLocalToAbsolute 
 , posAbsoluteToLocal
 , posLocalToAbsolute 
 , absoluteToLocal
 , localToAbsolute
 ) where

import Batteries
import Content.Facing
import Content.XY
import Debug.Trace


facingAbsoluteToLocal :: Facing -> XY -> XY
facingAbsoluteToLocal f (XY xy) = case f of
  South -> XY xy
  North -> XY { x: -xy.x, y: -xy.y }
  East  -> XY { x: -xy.y, y:  xy.x }
  West  -> XY { x:  xy.y, y: -xy.x }

facingLocalToAbsolute :: Facing -> XY -> XY
facingLocalToAbsolute f (XY xy) = case f of
  South -> XY xy
  North -> XY { x: -xy.x, y: -xy.y }
  East  -> XY { x:  xy.y, y: -xy.x }
  West  -> XY { x: -xy.y, y:  xy.x }

posAbsoluteToLocal :: XY -> XY -> XY
posAbsoluteToLocal (XY origin) (XY xy) = XY {x: xy.x - origin.x, y: xy.y - origin.y}


posLocalToAbsolute :: XY -> XY -> XY
posLocalToAbsolute (XY origin) (XY xy) = result
  where
    result = XY {x: xy.x + origin.x, y: xy.y + origin.y}

absoluteToLocal :: forall f. { facing :: Facing, location :: XY | f } -> XY -> XY
absoluteToLocal x = facingAbsoluteToLocal x.facing <<< posAbsoluteToLocal x.location

localToAbsolute :: forall f. { facing :: Facing, location :: XY | f } -> XY -> XY
localToAbsolute x = posLocalToAbsolute x.location <<< facingLocalToAbsolute x.facing
