module GameState where

import Batteries

import Box
import PlayerData (Player(..), dummyPlayer)
import Content.Enemies (Enemy, dummyEnemy)
import Data.Maybe
import Data.Identity
import Control.Monad.Except.Trans
import Optic.Lens
import Optic.Types
import XY
import Data.Either


newtype GameState = GameState
  { player :: Player
  , enemies :: Array Enemy
  , boundaries :: Box
  }

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where 
  show = genericShow

serialize :: GameState -> String
serialize g = genericEncodeJSON defaultOptions g

deserialize :: String -> Maybe GameState
deserialize s = case un Identity (runExceptT (genericDecodeJSON defaultOptions s)) of
  Right x -> Just x
  Left _ -> Nothing

setLocation :: GameState -> XY -> GameState
setLocation (GameState g) l = GameState g'
  where
    (Player p) = g.player
    p' = p {location = l}
    g' = g {player = Player p'}

getLocation :: GameState -> XY
getLocation (GameState g) = p.location
  where
    (Player p) = g.player

locationLens :: Lens' GameState XY
locationLens = lens (getLocation) (setLocation)

dummyGameState :: GameState
dummyGameState = GameState { player: dummyPlayer, enemies: [dummyEnemy], boundaries: Box {width: 6, height: 6} }
