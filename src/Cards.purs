module Cards where

import Batteries

import Content.Cards (Card(..), Hand, dummyCard, CardEffect(..), card)
import PlayerData (Player(..))
import Facing (Facing(..), rotate)
import Content.Enemies
import XY (XY(..), fst, snd)
import Box
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array(length, drop, (!!), deleteAt, filter, notElem, elem, zipWith, replicate)
import GameState
import Geometry
import Data.Newtype (un)
import Control.Monad.State (execState, modify)
import Data.Foldable (sequence_)

discardN :: Int -> Hand -> Maybe Hand
discardN n h = if (length h) < n then Nothing else Just (drop n h)

discard :: Hand -> Maybe Hand
discard = discardN 1

play :: GameState -> Int -> GameState
play (GameState g) i = if (GameState g) /= g' then handleCardEffect g' (Card c') else (GameState g)
    where 
        h = g.hand
        (Card c') = fromMaybe dummyCard (map card $ h !! i)
        h' = deleteAt i h
        h'' = fromMaybe h (fromMaybe (pure h) (discardN <$> pure c'.cost <*> h'))
        g' = GameState (g {hand = h''})

-- origin is the upper left, x increases to the right, y increases down
effectCoordinates :: Player -> Int -> Array XY -> Array XY
effectCoordinates (Player p) range area = do
  xc <- xcoords
  yc <- ycoords
  pure $ XY {x: xc, y: yc}
    where 
        ploc :: { x :: Int, y :: Int }
        ploc  = un XY p.location
        effectCenter = case p.facing of
                         East -> XY {x: ploc.x + range, y: ploc.y}
                         West -> XY {x: ploc.x - range, y: ploc.y}
                         North -> XY {x: ploc.x, y: ploc.y - range}
                         South -> XY {x: ploc.x, y: ploc.y + range}
        xoffsets = map fst area
        xcoords = zipWith (+) xoffsets (replicate (length xoffsets) (fst effectCenter))
        yoffsets = map snd area
        ycoords = zipWith (+) yoffsets (replicate (length yoffsets) (snd effectCenter))

handleCardEffect :: GameState -> Card -> GameState
handleCardEffect g (Card c) = flip execState g $ do
  sequence_ $ map (\e -> modify (handle1CardEffect e)) c.effect

handle1CardEffect :: CardEffect -> GameState -> GameState
handle1CardEffect (Attack a) (GameState g) = GameState $ g { enemies = filter enemyFilter g.enemies }
    where enemyFilter (Enemy e) = e.location `notElem` (effectCoordinates g.player a.range a.area)

handle1CardEffect (Move xy) gs@(GameState g) = movePlayerTo targetLocation gs
  where
    targetLocation = localToAbsolute (un Player g.player) xy

handle1CardEffect (Rotate i) (GameState g) = GameState g'
  where
    r :: Player -> Player
    r = over Player $ \p ->p{facing = rotate i p.facing}
    newplayer = r g.player
    g' = g{ player = r g.player }

handle1CardEffect (AttackMove xy) gs@(GameState g) = 
  case enemyAtLocation targetLocation gs of
    Nothing -> movePlayerTo targetLocation gs
    Just _ -> removeEnemyAtLocation targetLocation gs
  where 
    player = un Player g.player
    targetLocation = localToAbsolute player xy


handle1CardEffect _ g = g



enemyAtLocation :: XY -> GameState -> Maybe Enemy
enemyAtLocation l (GameState g) = head $ filter (\(Enemy e) -> e.location == l) g.enemies

movePlayerTo :: XY -> GameState -> GameState
movePlayerTo xy@(XY l) (GameState g) = GameState g{ player = p' }
  where
    p' = over Player ( _{ location = xy } ) g.player
    (Box b) = g.boundaries
    legal = (b.xmin <= l.x) && (b.xmax >= l.x) && (b.ymin <= l.y) && (b.ymax >= l.y)

removeEnemyAtLocation :: XY -> GameState -> GameState
removeEnemyAtLocation xy (GameState g) = GameState g{ enemies = filter (\(Enemy e) -> e.location /= xy) g.enemies }




