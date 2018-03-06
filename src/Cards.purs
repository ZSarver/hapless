module Cards where

import Prelude
import CardData (Card(..), Hand, dummyCard, CardEffect(..))
import PlayerData
import Facing
import EnemyData
import XY
import Box
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array(length, drop, (!!), deleteAt, filter, notElem, elem, zipWith, replicate)
import GameState
import Data.Newtype (un)

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
effectCoordinates :: Player -> Card -> Array XY
effectCoordinates (Player p) (Card c) = do
  xc <- xcoords
  yc <- ycoords
  pure $ XY {x: xc, y: yc}
    where 
        ploc :: { x :: Int, y :: Int }
        ploc  = un XY p.location
        effectCenter
            | p.facing == East = XY {x: ploc.x + c.range, y: ploc.y}
            | p.facing == West = XY {x: ploc.x - c.range, y: ploc.y}
            | p.facing == North = XY {x: ploc.x, y: ploc.y - c.range}
            | p.facing == South = XY {x: ploc.x, y: ploc.y + c.range}
            | otherwise = XY {x: ploc.x, y: ploc.y}
        xoffsets = map fst c.area
        xcoords = zipWith (+) xoffsets (replicate (length xoffsets) (fst effectCenter))
        yoffsets = map snd c.area
        ycoords = zipWith (+) yoffsets (replicate (length yoffsets) (snd effectCenter))

handleCardEffect :: GameState -> Card -> GameState
handleCardEffect g card@(Card c)
    | Attack `elem` c.effect = handleAttackEffect g card
    | Move `elem` c.effect = handleMoveEffect g card
    | otherwise = g

handleAttackEffect :: GameState -> Card -> GameState
handleAttackEffect (GameState g) c = GameState $ g { enemies = filter enemyFilter g.enemies }
    where
        enemyFilter (Enemy e) = e.location `notElem` (effectCoordinates g.player c)

handleMoveEffect :: GameState -> Card -> GameState
handleMoveEffect (GameState g) c = if canMove then moved else (GameState g)
    where
        (Player p) = g.player
        (XY l) = p.location
        (Box b) = g.boundaries
        canMove
            | p.facing == North && l.y == 1 = false
            | p.facing == South && l.y == b.height-1 = false
            | p.facing == East && l.x == b.width-1 = false
            | p.facing == West && l.x == 1 = false
            | otherwise = true
        moved
            | p.facing == North = setLocation (GameState g) (XY {x: l.x, y: (l.y - 1)})
            | p.facing == South = setLocation (GameState g) (XY {x: l.x, y: (l.y + 1)})
            | p.facing == East = setLocation (GameState g) (XY {x: (l.x + 1), y: l.y})
            | p.facing == West = setLocation (GameState g) (XY {x: (l.x - 1), y: l.y})
            | otherwise = (GameState g)
