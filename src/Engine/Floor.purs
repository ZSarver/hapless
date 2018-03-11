module Engine.Floor where

import Batteries
import Engine.Engine
import Core.GameState
import Content.Enemies
import Content.XY
import Content.Facing
import Core.Box
import Core.Player

import Control.Monad.Eff(Eff(..))
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Eff.Random
import Data.Array (replicate)

advanceFloor ∷ ∀ e. Int → Engine e Unit
advanceFloor n = do
  (GameState g) ← get
  let (Player p) = g.player
  -- we need to generate new stuff
  enemies' ← enemies_ g
  stairs' ← stairs_ g
  put $ if p.location == g.stairs then GameState (g {enemies = enemies', stairs = stairs', floor = n}) else GameState g
  where
    -- boundaries_ g = liftEff $ do
    --   xmax' ← randomInt (n + 4) (n + 8)
    --   ymax' ← randomInt (n + 4) (n + 8)
    --   pure $ Box {xmin: 1, xmax: xmax', ymin: 1, ymax: ymax'}
    enemies_ g = liftEff $ do
      let n = g.floor
      numEnemies ← randomInt n (n + 2)
      sequence $ replicate numEnemies (genRandomEnemy (GameState g))
    stairs_ g = liftEff $ do
      let (Box b) = g.boundaries
      x' ← randomInt b.xmin b.xmax
      y' ← randomInt b.ymin b.ymax
      pure $ XY {x: x', y: y'}
      

genRandomEnemy ∷ ∀ e. GameState → Eff (random ∷ RANDOM | e) Enemy
genRandomEnemy (GameState g) = do
  species' ← species_
  loc ← loc_ g
  facing' ← facing_
  pure $ Enemy {species: species', location: loc, facing: facing'}
  where
    species_ = do
      n ← randomInt 1 3
      case n of
        1 → pure Skeleton
        2 → pure Ghost
        _ → pure Slime
    loc_ g = do
      let (Box b) = g.boundaries
      x' ← randomInt b.xmin b.xmax
      y' ← randomInt b.ymin b.ymax
      pure $ XY {x: x', y: y'}
    facing_ = do
      n ← randomInt 1 4
      case n of
        1 → pure North
        2 → pure South
        3 → pure East
        _ → pure West
      
