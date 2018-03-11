module Engine.Cards where

import Batteries

import Content.Cards (ShortCard, Card(..), Hand, dummyCard, CardEffect(..), card)
import Core
import Content.Enemies

import Data.Array(length, drop, (!!), deleteAt, filter, notElem, elem, zipWith, replicate)
import Control.Monad.State (execState, modify)
import Data.Foldable (sequence_)
import Debug.Trace (traceAny)
import Engine.Engine
import Engine.Deck

canPlay :: Card -> GameState -> Boolean
canPlay (Card c) (GameState g) = c.cost < (length g.hand)

removeCard :: Int -> Hand -> Hand
removeCard i hand = fromMaybe hand $ deleteAt i hand

play :: forall e. Int -> Engine e Boolean
play i = do
  gs@(GameState g) <- get
  case g.hand !! i of
    Nothing -> pure false
    Just sc -> 
      let c = card sc 
          t = canPlay c gs
      in do
         _ <- when t $ do 
           _ <- modify $ liftHand (removeCard i)
           discardN (un Card c).cost
           handleCardEffectE c
         pure t

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
        xoffsets = map x area
        xcoords = zipWith (+) xoffsets (replicate (length xoffsets) (x effectCenter))
        yoffsets = map y area
        ycoords = zipWith (+) yoffsets (replicate (length yoffsets) (y effectCenter))


handleCardEffectE :: forall e. Card -> Engine e Unit
handleCardEffectE (Card c) = sequence_ $ map (\e -> modify (handle1CardEffect e)) c.effect

handleCardEffect :: Card -> GameState -> GameState
handleCardEffect (Card c) g = flip execState g $ do
  sequence_ $ map (\e -> modify (handle1CardEffect e)) c.effect

handle1CardEffect :: CardEffect -> GameState -> GameState
handle1CardEffect (Attack a) (GameState g) = GameState $ g { enemies = filter enemyFilter g.enemies }
    where enemyFilter (Enemy e) = e.location `notElem` (effectCoordinates g.player a.range a.area)

handle1CardEffect (Move xy) gs@(GameState g) = movePlayerTo targetLocation gs
  where
    targetLocation = localToAbsolute (un Player g.player) xy

handle1CardEffect (Rotate i) gs = liftPlayer (\(Player p) -> Player p{facing = rotate i p.facing}) gs

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
removeEnemyAtLocation xy = liftEnemies $ filter (\(Enemy e) -> e.location /= xy)
