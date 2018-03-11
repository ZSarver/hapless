module Core.Util where

import Batteries
import Data.Array as Arr

pop :: forall a. (Eq a) => (a -> Boolean) -> Array a -> Maybe (Tuple a (Array a))
pop predicate array = do
  e <- find predicate array
  pure $ Tuple e (Arr.delete e array)

popAt :: forall a. Int -> Array a -> Maybe (Tuple a (Array a))
popAt index array = do
  e <- array !! index
  arr' <- Arr.deleteAt index array
  pure $ Tuple e arr'
