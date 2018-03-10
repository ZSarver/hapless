module Engine.Enemies where

import Batteries

import Content.Enemies (Enemy(..), MoveBehavior(..), AttackBehavior(..))
import Core

import Data.Array (zipWith, group, filter, concat)
import Data.Array as Arr
import Data.Variant (match)
import Data.NonEmpty as NE
import Control.MonadZero (guard)
import Control.Monad.State
import Control.Monad.Maybe.Trans


data Consequence
  = Move XY
  | Turn Facing
  | Noop

derive instance genericConsequence :: Generic Consequence _
instance eqConsequence :: Eq Consequence where eq = genericEq

interpretAction :: Enemy -> Action -> Consequence
interpretAction (Enemy e) Forward = Move $ localToAbsolute e forward
interpretAction (Enemy e) Left    = Turn $ rotate widdershins e.facing
interpretAction (Enemy e) Right   = Turn $ rotate clockwise e.facing
interpretAction (Enemy e) Pass    = Noop

interpretActions :: Enemy -> Array Action -> Array Consequence
interpretActions e actions = (mapAccumL step e actions).value
  where
  step e a = let v = interpretAction e a in
    { accum: applySelf v e, value: v }

applySelf :: Consequence -> Enemy -> Enemy
applySelf Noop e = e
applySelf (Turn f) (Enemy e) = Enemy e{ facing = f } 
applySelf (Move xy) (Enemy e) = Enemy e{ location = xy }


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
perform i c gs = liftEnemies (Arr.modifyAtIndices [i] (applySelf c)) gs

advanceEnemies :: Partial => GameState -> GameState
advanceEnemies gs@(GameState g) = result
  where
    -- Phase 1: for each enemy, determine if it wants to move, and where to
    actions = flip map g.enemies $ \e -> enemyAction g.bestiary e g.player
    foo = traceAny actions (\_ -> actions)
    consequences = zipWith interpretActions g.enemies foo 
    -- Phase 2: find enemies whose moves are in conflict 
    -- these have their Move commands turned into Noops
    targetTiles = concat consequences >>= case _ of
                                            Move m -> [m]
                                            _ -> []
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










