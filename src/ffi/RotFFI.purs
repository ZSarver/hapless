module RotFFI where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Aff
import Control.Monad.Aff.Compat
import Control.Monad.Eff.Class (liftEff)
import Data.StrMap (StrMap)

foreign import data ROT :: Effect
foreign import data RotInstance :: Type

newtype DisplayOptions = DisplayOptions
  { width  :: Int
  , height :: Int
  , tileSet :: String
  , tileMap :: StrMap {x :: Int, y :: Int}
  , tileSize :: Int
  }


foreign import _ready :: forall e. EffFnAff (rot :: ROT | e) Unit
ready :: forall e. Aff (rot :: ROT | e) Unit
ready = fromEffFnAff _ready

foreign import _initrotjs :: forall e. DisplayOptions -> Eff (rot :: ROT | e) RotInstance
initrotjs :: forall e. DisplayOptions -> Aff (rot :: ROT | e) RotInstance
initrotjs opts = do
  rotjs <- liftEff $ _initrotjs opts
  ready
  pure rotjs

foreign import _clear :: forall e. RotInstance -> Eff (rot :: ROT | e) Unit
clear :: forall e. RotInstance -> Aff (rot :: ROT | e) Unit
clear = liftEff <<< _clear

foreign import _putTile :: forall e. String -> Int -> Int -> RotInstance -> Eff (rot :: ROT | e) Unit
putTile :: forall e. String -> Int -> Int -> RotInstance -> Aff (rot :: ROT | e) Unit  
putTile t x y d = liftEff $ _putTile t x y d

foreign import _putTile2 :: forall e. String -> String -> Int -> Int -> RotInstance -> Eff (rot :: ROT | e) Unit
putTile2 :: forall e. String -> String -> Int -> Int -> RotInstance -> Aff (rot :: ROT | e) Unit  
putTile2 fg bg x y d = liftEff $ _putTile2 fg bg x y d

newtype Key = Key { keyCode :: String }
foreign import _getKey :: forall e. RotInstance -> EffFnAff (rot :: ROT | e) Key
getKey :: forall e. RotInstance -> Aff (rot :: ROT | e) Key
getKey = fromEffFnAff <<< _getKey







