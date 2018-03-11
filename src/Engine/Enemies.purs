module Engine.Enemies where

import Batteries

import Content.Enemies (Enemy(..), MoveBehavior(..), AttackBehavior(..))
import Core

import Core.Util
import Data.Array (zipWith, group, filter, concat)
import Data.Array as Arr
import Data.Variant (match, onMatch)
import Data.NonEmpty as NE
import Control.MonadZero (guard)
import Control.Monad.State
import Control.Monad.Maybe.Trans
import Engine.Engine
import Control.Monad.Rec.Class
import Control.Monad.Writer.Class (tell)
import Engine.Deck
import Content.Cards as Cards

data Consequence
  = Move XY
  | Turn Facing
  | Damage Int
  | Discard Int
  | Knockback Int
  | Deadcard Int
  | Noop

derive instance genericConsequence :: Generic Consequence _
instance eqConsequence :: Eq Consequence where eq = genericEq

interpretAction :: Bestiary -> Enemy -> Action -> Consequence
interpretAction _ (Enemy e) Forward = Move $ localToAbsolute e forward
interpretAction _ (Enemy e) Left    = Turn $ rotate widdershins e.facing
interpretAction _ (Enemy e) Right   = Turn $ rotate clockwise e.facing
interpretAction _ (Enemy e) Pass    = Noop
interpretAction b (Enemy e) Strike  = case attackBehavior b e.species of
  Smash -> Damage 1
  Rend -> Discard 2
  Befuddle -> Deadcard 2

interpretActions :: Bestiary -> Enemy -> Array Action -> Array Consequence
interpretActions b e actions = (mapAccumL step e actions).value
  where
  step e a = let v = interpretAction b e a in
    { accum: applySelf v e, value: v }


getEnemy :: forall e. Int -> Engine e (Maybe Enemy)
getEnemy i = flip map get $ \(GameState g) -> Arr.index g.enemies i

isAttack :: Consequence -> Boolean
isAttack (Damage _) = true
isAttack (Discard _) = true
isAttack (Deadcard _) = true
isAttack (Knockback _) = true
isAttack _ = false

logEnemyAction :: forall e. Int -> Consequence -> Engine e Unit
logEnemyAction i c 
  | isAttack c = getEnemy i >>= case _ of
    Nothing -> pure unit
    Just (Enemy e) -> do
      tell $ "The " <> show e.species <> " hits you. "
  | otherwise = pure unit

performE :: forall e. Int -> Consequence -> Engine e Unit
performE i c = do
  modify $ liftEnemies (Arr.modifyAtIndices [i] (applySelf c))
  logEnemyAction i c
  applyPlayer c
  when (isAttack c) $ newLine
 
applySelf :: Consequence -> Enemy -> Enemy
applySelf (Turn f) (Enemy e) = Enemy e{ facing = f } 
applySelf (Move xy) (Enemy e) = Enemy e{ location = xy }
applySelf _ e = e

applyPlayer :: forall e. Consequence -> Engine e Unit
applyPlayer (Damage n) = do
  modify $ liftHp $ (_ - n)
  tell $ "You take " <> show n <> " damage. "

applyPlayer (Discard n) = do
  (GameState g) <- get
  let h = Arr.length g.hand
  tell $ "You lose " <> show n <> " cards. "
  remainder <- discardN n
  when (remainder > 0) do 
    tell $ "You don't have enough cards! "
    applyPlayer (Damage remainder)

applyPlayer (Deadcard n) = do
  (GameState g) <- get
  let h = Arr.length g.hand
  tell $ "You gain " <> show n <> " cards. "
  remainder <- padN n Cards.Pass
  when (remainder > 0) $ do
    tell "You have too many cards! "
    applyPlayer (Damage remainder)
  
  


applyPlayer _ = pure unit

isLegal :: GameState -> Consequence -> Boolean
isLegal gs (Move xy) = onMatch { empty: \_ -> true } (const false) (at gs xy)
isLegal _ _ = true


advanceEnemies :: forall e. Engine e Unit
advanceEnemies = do
  gs@(GameState g) <- get
  let -- Phase 1: for each enemy, determine if it wants to move, and where to
      actions = flip map g.enemies $ \e -> enemyAction g.bestiary e g.player
      consequences = zipWith (interpretActions g.bestiary) g.enemies actions 
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
  tailRecM step $ Arr.zip (Arr.range 0 $ Arr.length consequences') consequences'

type Acc = Array (Tuple Int (Array Consequence)) 
step :: forall e. Acc -> Engine e (Step Acc Unit)
step acc = do
  gameState <- get
  let next = pop (\(Tuple _ cs) -> case head cs of   -- find an enemy with pending legal move or nothing left to do
        Nothing -> true
        Just x -> isLegal gameState x) acc
  case next of
    Nothing -> pure $ Done unit                              -- if we didnt find any, exit
    Just (Tuple (Tuple ix cs) acc') ->
      case pop (const true) cs of
        Nothing -> pure $ Loop acc'                          -- nothing to do? remove him from queue
        Just (Tuple c cs') -> do
          performE ix c                                      -- do one pending move and
          pure $ Loop (Arr.cons (Tuple ix cs') acc')         -- put him back in rotation








