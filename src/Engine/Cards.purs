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
           handleCardEffect c
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

kill :: forall e. Enemy -> Engine e Unit
kill (Enemy e) = do
  tell $ "You kill the " <> show e.species <> "."
  modify $ removeEnemyAt (e.location)

handleCardEffect :: forall e. Card -> Engine e Unit
handleCardEffect (Card c) = sequence_ $ map handle1CardEffect c.effect

handle1CardEffect :: forall e. CardEffect -> Engine e Unit
handle1CardEffect (Attack a) = do
  gs@(GameState g) <- get
  let targetSpaces = effectCoordinates g.player a.range a.area
      targets = map (at gs) targetSpaces
  sequence_ $ flip map targets $ onMatch
    { enemy: \e -> kill e }
    (const $ pure unit)

handle1CardEffect (AttackMove xy)  = do
  gs@(GameState g) <- get
  let targetLocation = localToAbsolute (un Player g.player) xy
  onMatch 
    { enemy: \e -> kill e
    , empty: const $ modify $ flip movePlayerTo targetLocation 
    } (const $ pure unit) (at gs targetLocation)

handle1CardEffect (Rotate i) = modify $ liftPlayer (\(Player p) -> Player p{facing = rotate i p.facing})
handle1CardEffect (Move xy) = do
  (GameState g) <- get
  modify $ flip movePlayerTo $ localToAbsolute (un Player g.player) xy
