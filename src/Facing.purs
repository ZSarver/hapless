module Facing where

import Prelude
import Data.Generic (class Generic, gEq)

data Facing = North | South | East | West

derive instance genericFacing :: Generic Facing
instance eqFacing :: Eq Facing where
  eq = gEq


