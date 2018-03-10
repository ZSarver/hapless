module Engine.Enemies where

import Batteries

import Content.Enemies (Enemy(..), MoveBehavior(..), AttackBehavior(..))
import Core

import Data.Array (zipWith, group, filter, concat)
import Data.Array as Arr
import Data.Variant (match, onMatch)
import Data.NonEmpty as NE
import Control.MonadZero (guard)
import Control.Monad.State
import Control.Monad.Maybe.Trans


data Consequence
  = Move XY
  | Turn Facing
  | Damage Int
  | Noop

derive instance genericConsequence :: Generic Consequence _
instance eqConsequence :: Eq Consequence where eq = genericEq

interpretAction :: Enemy -> Action -> Consequence
interpretAction (Enemy e) Forward = Move $ localToAbsolute e forward
interpretAction (Enemy e) Left    = Turn $ rotate widdershins e.facing
interpretAction (Enemy e) Right   = Turn $ rotate clockwise e.facing
interpretAction (Enemy e) Pass    = Noop
interpretAction (Enemy e) Strike  = Damage 1

interpretActions :: Enemy -> Array Action -> Array Consequence
interpretActions e actions = (mapAccumL step e actions).value
  where
  step e a = let v = interpretAction e a in
    { accum: applySelf v e, value: v }


perform :: Int -> Consequence -> GameState -> GameState
perform i c gs = flip execState gs $ do 
  modify $ liftEnemies (Arr.modifyAtIndices [i] (applySelf c))
  modify $ liftHp (applyDamage c)
 

applySelf :: Consequence -> Enemy -> Enemy
applySelf (Turn f) (Enemy e) = Enemy e{ facing = f } 
applySelf (Move xy) (Enemy e) = Enemy e{ location = xy }
applySelf _ e = e

applyDamage :: Consequence -> Int -> Int
applyDamage (Damage n) hp = hp - n
applyDamage _ hp = hp



isLegal :: GameState -> Consequence -> Boolean
isLegal gs (Move xy) = onMatch { empty: \_ -> true } (const false) (at gs xy)
isLegal _ _ = true


pop :: forall a. (Eq a) => (a -> Boolean) -> Array a -> Maybe (Tuple a (Array a))
pop predicate array = do
  e <- find predicate array
  pure $ Tuple e (Arr.delete e array)



advanceEnemies :: Partial => GameState -> GameState
advanceEnemies gs@(GameState g) = result
  where
    -- Phase 1: for each enemy, determine if it wants to move, and where to
    actions = flip map g.enemies $ \e -> enemyAction g.bestiary e g.player
    consequences = zipWith interpretActions g.enemies actions 
    -- Phase 2: find enemies whose moves are in conflict 
    -- these have their Move commands turned into Noops
    targetTiles = concat consequences >>= case _ of
                                            Move m -> [m]
                                            _ -> []
    badMoves = map Move $ Arr.cons (un Player g.player).location $ do
      t <- group targetTiles
      guard (length t > 1)
      pure $ NE.head t

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










