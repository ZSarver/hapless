module Engine.Engine where

import Batteries
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import FFI.DOM
import Control.Monad.State
import Core.GameState
import Control.Monad.Writer.Class
import Control.Monad.Rec.Class

newtype Engine e a = Engine (StateT GameState (Aff (dom :: DOM | e)) a)

-- (dom :: DOM | e)
derive instance newtypeEngine :: Newtype (Engine e a) _

instance functorEngine :: Functor (Engine e) where
  map f (Engine e) = Engine (map f e)

instance applyEngine :: Apply (Engine e) where
  apply = ap

instance applicativeEngine :: Applicative (Engine e) where
  pure = Engine <<< pure

instance bindEngine :: Bind (Engine e) where
  bind (Engine x) f = Engine $ x >>= (f >>> un Engine)

instance monadEngine :: Monad (Engine e) where

instance monadStateEngine :: MonadState GameState (Engine e) where
  state f = Engine $ state f

instance monadTellEngine :: MonadTell String (Engine e) where
  tell s = Engine $ lift $ liftEff $ combatLog s
    
instance monadRecEngine :: MonadRec (Engine e) where
  tailRecM f a = Engine $ tailRecM (f >>> un Engine) a

instance monadEffEngine :: MonadEff (dom :: DOM | e) (Engine e) where
  liftEff eff = Engine $ liftEff eff

instance monadAffEngine :: MonadAff (dom :: DOM | e) (Engine e) where
  liftAff aff = Engine $ liftAff aff

