module Cards where

import Prelude
import CardData (Card(..), Hand, dummyCard)
import PlayerData
import EnemyData
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array(length, drop, (!!), deleteAt, (..), zipWith, replicate)
import Math(ceil)
import Data.Int(toNumber, floor)

discardN :: Int -> Hand -> Maybe Hand
discardN n h = if (length h) < n then Nothing else Just (drop n h)

discard :: Hand -> Maybe Hand
discard = discardN 1

play :: Int -> Hand -> Hand
play i h = fromMaybe h (fromMaybe (pure h) (discardN <$> pure c'.cost <*> h'))
    where 
        (Card c') = fromMaybe dummyCard (h !! i)
        h' = deleteAt i h

-- origin is the upper left, x increases to the right, y increases down
effectCoordinates :: Player -> Card -> Array {x :: Int, y :: Int}
effectCoordinates (Player p) (Card c) = do
  xc <- xcoords
  yc <- ycoords
  pure {x: xc, y: yc}
    where 
        effectCenter
            | p.facing == East = {x: p.location.x + c.range, y: p.location.y}
            | p.facing == West = {x: p.location.x - c.range, y: p.location.y}
            | p.facing == North = {x: p.location.x, y: p.location.y - c.range}
            | p.facing == South = {x: p.location.x, y: p.location.y + c.range}
            | otherwise = {x: p.location.x, y: p.location.y}
        xoffsets = map ((-) (ceil ((toNumber c.area.width) / 2.0))) (map toNumber (1..c.area.width))
        xcoords = zipWith (+) (map floor xoffsets) (replicate (length xoffsets) effectCenter.x)
        yoffsets = map ((-) (ceil ((toNumber c.area.height) / 2.0))) (map toNumber (1..c.area.height))
        ycoords = zipWith (+) (map floor yoffsets) (replicate (length yoffsets) effectCenter.y)