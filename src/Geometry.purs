module Geometry
 ( facingAbsoluteToLocal
 , facingLocalToAbsolute 
 , posAbsoluteToLocal
 , posLocalToAbsolute 
 , absoluteToLocal
 , localToAbsolute
 ) where

import Batteries
import Facing
import XY

wrap z = { location : XY z}

facingAbsoluteToLocal :: Facing -> { location :: XY } -> { location :: XY }
facingAbsoluteToLocal f p = case f of
  South -> wrap xy
  North -> wrap { x: -xy.x, y: -xy.y }
  East  -> wrap { x:  xy.y, y: -xy.x }
  West  -> wrap { x: -xy.y, y:  xy.x }
  where
    XY xy = p.location

facingLocalToAbsolute :: Facing -> { location :: XY } -> { location :: XY }
facingLocalToAbsolute f p = case f of
  South -> wrap xy
  North -> wrap { x: -xy.x, y: -xy.y }
  East  -> wrap { x: -xy.y, y:  xy.x }
  West  -> wrap { x:  xy.y, y: -xy.x }
  where
    XY xy = p.location

posAbsoluteToLocal :: XY -> { location :: XY } -> { location :: XY }
posAbsoluteToLocal (XY origin) p = wrap {x: xy.x - origin.x, y: xy.y - origin.y}
  where
    XY xy = p.location

posLocalToAbsolute :: XY -> { location :: XY } -> { location :: XY }
posLocalToAbsolute (XY origin) p = wrap {x: xy.x + origin.x, y: xy.y + origin.y}
  where
    XY xy = p.location

absoluteToLocal :: { facing :: Facing, location :: XY } -> { location :: XY } -> { location :: XY }
absoluteToLocal x = facingAbsoluteToLocal x.facing <<< posAbsoluteToLocal x.location

localToAbsolute :: { facing :: Facing, location :: XY } -> { location :: XY } -> { location :: XY }
localToAbsolute x = posLocalToAbsolute x.location <<< facingLocalToAbsolute x.facing
