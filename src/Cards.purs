module Cards where

import Prelude
import CardData (Card(..), Hand, dummyCard, CardEffect(..))
import PlayerData
import Facing
import EnemyData
import XY
import Box
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array(length, drop, (!!), deleteAt, (..), zipWith, replicate, filter, notElem, elem)
import Math(ceil)
import Data.Int(toNumber, floor)
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
        area = un Box (c.area)
        xoffsets = map ((-) (ceil ((toNumber area.width) / 2.0))) (map toNumber (1..area.width))
        xcoords = zipWith (+) (map floor xoffsets) (replicate (length xoffsets) (un XY effectCenter).x)
        yoffsets = map ((-) (ceil ((toNumber area.height) / 2.0))) (map toNumber (1..area.height))
        ycoords = zipWith (+) (map floor yoffsets) (replicate (length yoffsets) (un XY effectCenter).y)

handleCardEffect :: GameState -> Card -> GameState
handleCardEffect g card@(Card c)
    | Attack `elem` c.effect = handleAttackEffect g card
    -- | Move `elem` c.effects = handleMoveEffect (GameState g) (Card c)
    | otherwise = g

handleAttackEffect :: GameState -> Card -> GameState
handleAttackEffect (GameState g) c = GameState $ g { enemies = filter enemyFilter g.enemies }
    where
        enemyFilter (Enemy e) = e.location `notElem` (effectCoordinates g.player c)

-- handleMoveEffect :: GameState -> Card -> GameState
-- handleMoveEffect (GameState g) (Card c) = if canMove then move else GameState g
--     where
--         canMove = 
