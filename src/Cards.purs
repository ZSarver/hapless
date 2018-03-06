module Cards where

import Prelude
import CardData (Card(..), Hand, dummyCard, CardEffect(..))
import Coordinate
import PlayerData
import Facing
import EnemyData
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array(length, drop, (!!), deleteAt, filter, notElem, elem, zipWith, replicate)
import GameState

discardN :: Int -> Hand -> Maybe Hand
discardN n h = if (length h) < n then Nothing else Just (drop n h)

discard :: Hand -> Maybe Hand
discard = discardN 1

play :: Int -> Hand -> Hand
play i h = fromMaybe h (fromMaybe (pure h) (discardN <$> pure c'.cost <*> h'))
    where 
        c' = fromMaybe dummyCard (h !! i)
        h' = deleteAt i h

-- origin is the upper left, x increases to the right, y increases down
effectCoordinates :: Player -> Card -> Array Coordinate
effectCoordinates p c = do
  xc <- xcoords
  yc <- ycoords
  pure $ Coordinate {x: xc, y: yc}
    where 
        effectCenter
            | p.facing == East = {x: p.location.x + c.range, y: p.location.y}
            | p.facing == West = {x: p.location.x - c.range, y: p.location.y}
            | p.facing == North = {x: p.location.x, y: p.location.y - c.range}
            | p.facing == South = {x: p.location.x, y: p.location.y + c.range}
            | otherwise = {x: p.location.x, y: p.location.y}
        xoffsets = map fst c.area
        xcoords = zipWith (+) xoffsets (replicate (length xoffsets) effectCenter.x)
        yoffsets = map snd c.area
        ycoords = zipWith (+) yoffsets (replicate (length yoffsets) effectCenter.y)

handleCardEffect :: GameState -> Card -> GameState
handleCardEffect g c
    | Attack `elem` c.effect = handleAttackEffect g c
    | Move `elem` c.effect = handleMoveEffect g c
    | otherwise = g

handleAttackEffect :: GameState -> Card -> GameState
handleAttackEffect g c = g { enemies = filter enemyFilter g.enemies }
    where
        enemyFilter = \e -> (Coordinate e.location) `notElem` (effectCoordinates g.player c)

handleMoveEffect :: GameState -> Card -> GameState
handleMoveEffect g _ = g
-- handleMoveEffect g c = if canMove then moved else g
--     where
--         canMove
--             | g.player.facing == North && g.player.location.y == 1 = false
--             | g.player.facing == South && g.player.location.y == g.boundaries.height-1 = false
--             | g.player.facing == East && g.player.location.x == g.boundaries.width-1 = false
--             | g.player.facing == West && g.player.location.x == 1 = false
--             | otherwise = true
--         moved
--             -- | g.player.facing == North = g { player { location { y = g.player.location.y - 1 } } }
--             | otherwise g
