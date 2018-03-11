module Core.Render where

import Batteries

import Core.GameState (GameState(..))
import Core.Player(Player(..))
import Core.Deck (drawPile, discardPile)
import Content.Enemies (Enemy(..), Species(..))
import Content.Cards (ShortCard, card, Card(..))
import Content.Tiles as T
import Content.Facing (Facing(..))
import Content.XY (XY(..))
import FFI.Rot (ROT, RotInstance, clear, putTile, putTile2)
import FFI.DOM
import Control.Monad.Aff (Aff)
import Data.Array (concat, range, zip, (..))
import Data.Traversable (sequence_)
import Control.Monad.Eff.Class (liftEff)


placeWalls :: forall e. RotInstance -> Aff (rot :: ROT | e) Unit
placeWalls rotjs = sequence_ $ map render1wall wallspaces
  where 
    render1wall (XY s) = putTile T.wall s.x s.y rotjs
    wallspaces = concat [top, bottom, left, right]
    top    = map (\x -> XY {y: 0, x: x}) $ range 0 7
    bottom = map (\x -> XY {y: 7, x: x}) $ range 0 7
    left   = map (\y -> XY {x: 0, y: y}) $ range 1 6
    right  = map (\y -> XY {x: 7, y: y}) $ range 1 6

placeFloor :: forall e. RotInstance -> Aff (rot :: ROT | e) Unit
placeFloor rotjs = sequence_ $ map render1floor floorspaces
  where
    render1floor s = putTile T.floor s.x s.y rotjs
    floorspaces = do
      x <- range 1 6
      y <- range 1 6
      pure {x: x, y: y}

placeDownStairs :: forall e. XY -> RotInstance -> Aff(rot :: ROT | e) Unit
placeDownStairs (XY xy) rotjs = putTile T.downStairs xy.x xy.y rotjs

render :: forall e. GameState -> RotInstance -> Aff (rot :: ROT, dom :: DOM | e) Unit
render (GameState gs) rotjs = do
  clear rotjs
  placeWalls rotjs
  placeFloor rotjs
  placeDownStairs (gs.stairs) rotjs
  renderPlayer gs.player rotjs
  renderCards gs.hand 
  liftEff $ displayFloor gs.floor
  liftEff $ displayHp gs.hp
  liftEff $ displayDeck (length $ drawPile gs.deck)
  liftEff $ displayDiscard (length $ discardPile gs.deck)
  sequence_ $ map (flip renderEnemy rotjs) gs.enemies

renderPlayer :: forall e. Player -> RotInstance -> Aff (rot :: ROT | e) Unit
renderPlayer (Player p) rotjs = putTile2 img T.floor x y rotjs
  where 
    (XY loc) = p.location
    x = loc.x
    y = loc.y
    img = case p.facing of
            North -> T.playerUp
            South -> T.playerDown
            East -> T.playerRight
            West -> T.playerLeft

renderCards :: forall e. Array ShortCard -> Aff (dom :: DOM | e) Unit
renderCards cards = liftEff do
  sequence_ $ map clearCardText (1..10)
  sequence_ $ map render1card $ zip cards (1..10)
  where
    render1card (Tuple c i) = putCardText i (show i <> ". " <> show c <> " |" <> show (un Card $ card c).cost <> "|")

renderEnemy :: forall e. Enemy -> RotInstance -> Aff (rot :: ROT | e) Unit
renderEnemy (Enemy e) rotjs = putTile2 img T.floor loc.x loc.y rotjs
  where
    (XY loc) = e.location
    img = enemyImage e.species e.facing
    enemyImage Skeleton North = T.skeletonUp
    enemyImage Skeleton South = T.skeletonDown
    enemyImage Skeleton West = T.skeletonLeft
    enemyImage Skeleton East = T.skeletonRight
    enemyImage Ghost North = T.ghostUp
    enemyImage Ghost South = T.ghostDown
    enemyImage Ghost West = T.ghostLeft
    enemyImage Ghost East = T.ghostRight
    enemyImage Slime North = T.slimeUp
    enemyImage Slime South = T.slimeDown
    enemyImage Slime West = T.slimeLeft
    enemyImage Slime East = T.slimeRight
