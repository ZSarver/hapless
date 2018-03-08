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

wrap z = { position : XY z}

facingAbsoluteToLocal :: Facing -> { position :: XY } -> { position :: XY }
facingAbsoluteToLocal f p = case f of
  South -> wrap xy
  North -> wrap { x: -xy.x, y: -xy.y }
  East  -> wrap { x:  xy.y, y: -xy.x }
  West  -> wrap { x: -xy.y, y:  xy.x }
  where
    XY xy = p.position

facingLocalToAbsolute :: Facing -> { position :: XY } -> { position :: XY }
facingLocalToAbsolute f p = case f of
  South -> wrap xy
  North -> wrap { x: -xy.x, y: -xy.y }
  East  -> wrap { x: -xy.y, y:  xy.x }
  West  -> wrap { x:  xy.y, y: -xy.x }
  where
    XY xy = p.position

posAbsoluteToLocal :: XY -> { position :: XY } -> { position :: XY }
posAbsoluteToLocal (XY origin) p = wrap {x: xy.x - origin.x, y: xy.y - origin.y}
  where
    XY xy = p.position

posLocalToAbsolute :: XY -> { position :: XY } -> { position :: XY }
posLocalToAbsolute (XY origin) p = wrap {x: xy.x + origin.x, y: xy.y + origin.y}
  where
    XY xy = p.position

absoluteToLocal :: { facing :: Facing, position :: XY } -> { position :: XY } -> { position :: XY }
absoluteToLocal x = facingAbsoluteToLocal x.facing <<< posAbsoluteToLocal x.position

localToAbsolute :: { facing :: Facing, position :: XY } -> { position :: XY } -> { position :: XY }
localToAbsolute x = posLocalToAbsolute x.position <<< facingLocalToAbsolute x.facing



