module Engine.Enemies where

import Batteries

import Content.Enemies (Enemy(..), MoveBehavior(..), AttackBehavior(..))
import Core

import Data.Ord
import Data.Map (lookup)
import Partial
import Data.Array (zipWith, group, filter, concat)
import Data.Array as Arr
import Data.Variant (onMatch, case_, match)
import Data.NonEmpty as NE
import Control.MonadZero (guard)
import Control.Monad.State
import Control.Monad.Maybe.Trans


data Action
  = Forward
  | Left
  | Right
  | Pass


data Consequence
  = Move XY
  | Turn Facing
  | Noop

derive instance genericConsequence :: Generic Consequence _
instance eqConsequence :: Eq Consequence where eq = genericEq

interpretAction :: Enemy -> Action -> Consequence
interpretAction (Enemy e) Forward = Move $ localToAbsolute e forward
interpretAction (Enemy e) Left    = Turn $ rotLeft e.facing
interpretAction (Enemy e) Right   = Turn $ rotRight e.facing
interpretAction (Enemy e) Pass    = Noop

isLegal :: GameState -> Consequence -> Boolean
isLegal _ Noop = true
isLegal _ (Turn _) = true
isLegal gs (Move xy) = flip match (at gs xy)
  { player: \(Player p) -> true
  , enemy: \(Enemy e) -> false
  , empty: \_ -> true
  } 

pop :: forall a. (Eq a) => (a -> Boolean) -> Array a -> Maybe (Tuple a (Array a))
pop predicate array = do
  e <- find predicate array
  pure $ Tuple e (Arr.delete e array)

perform :: Int -> Consequence -> GameState -> GameState
perform _ Noop g = g
perform i (Turn f) gs = liftEnemies (Arr.modifyAtIndices [i] turn) gs
  where turn (Enemy e) =  Enemy e{ facing = f} 
perform i (Move xy) gs = liftEnemies (Arr.modifyAtIndices [i] mv) gs
  where mv (Enemy e)  = Enemy e{ location = xy }

advanceEnemies :: Partial => GameState -> GameState
advanceEnemies gs@(GameState g) = result
  where
    -- Phase 1: for each enemy, determine if it wants to move, and where to
    actions = flip map g.enemies $ \e -> enemyAction g.bestiary e g.player
    consequences = zipWith (map <<< interpretAction) g.enemies actions 
    -- Phase 2: find enemies whose moves are in conflict 
    -- these have their Move commands turned into Noops
    targetTiles = do
      (Move c) <- concat consequences
      pure c
    badMoves = do
      t <- group targetTiles
      guard (length t > 1)
      pure $ Move $ NE.head t
    consequences' = flip (map <<< map) consequences $ \c -> 
                    if elem c badMoves
                      then Noop
                      else c
    -- Phase 3: loop:
    -- accumulator: indices of enemies with pending actions
        -- each step, check if one of them has a legal move
        -- if so, move that enemy, shift it to the other side, and repeat
        -- otherwise, halt
    loop :: forall a b. (b -> State a (Maybe b)) -> b -> State a b
    loop step g = maybe (pure g) (loop step) =<< (step g)

    step :: GameState -> State (Array (Tuple Int (Array Consequence))) (Maybe GameState)
    step gameState =  do
      acc <- get
      let next = pop (\(Tuple _ cs) -> case head cs of   -- find an enemy with pending legal move or nothing left to do
            Nothing -> true
            Just x -> isLegal gameState x) acc
      case next of
        Nothing -> pure Nothing                        -- if we didnt find any, exit
        Just (Tuple (Tuple ix cs) acc') ->
          case pop (const true) cs of
            Nothing -> do
              put acc'                                -- nothing to do? remove him from queue
              pure $ Just gameState                   -- and leave state as it was
            Just (Tuple c cs') -> do
              put (Arr.cons (Tuple ix cs') acc')      -- do one pending move and
              pure $ Just (perform ix c gameState)    -- put him back in rotation

    initial = Arr.zip (Arr.range 0 $ Arr.length consequences') consequences'
    result = evalState (loop step gs) initial

enemyAction :: Partial => Bestiary -> Enemy -> Player -> Array Action
enemyAction (Bestiary b) (Enemy e) (Player p) = move behavior (absoluteToLocal e p.location)
  where
    behavior = case (lookup e.species b) of
                 Just (Tuple atk mv) -> mv
                 Nothing -> crashWith $ "Species " <> show e.species <> " missing from bestiary."

turnToward :: XY -> Action
turnToward (XY xy)
  | xy.x >= 0 = Left
  | otherwise = Right


-- If it can move forward without increasing distance, it does so
-- turns quickly
-- confused if player is directly behind
move :: MoveBehavior -> XY -> Array Action
move Steadfast (XY xy) 
  | xy.y > 0  = [Forward]
  | xy.x > 0  = [Left, Forward]
  | xy.x < 0  = [Right, Forward]
  | otherwise = []

-- Always tries to reduce the greater of horizontal or vertical distance
-- turns slowly
move Waffly target@(XY xy) 
  | xy.y < (abs xy.x) = [turnToward target]
  | xy.y > 0          = [Forward]
  | otherwise         = [turnToward target]

-- Does not turn left
-- turns quickly
move Righty (XY xy)
  | xy.y > 0  = [Forward]
  | xy.x < 0  = [Right, Forward]
  | otherwise = [Right, Right]











