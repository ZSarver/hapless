module GameState where

import Batteries

import Box
import PlayerData (Player(..), dummyPlayer)
import Content.Enemies (Enemy, dummyEnemy)
import Bestiary
import Content.Cards (Hand, ShortCard (..))
import Data.Maybe
import Data.Identity
import Control.Monad.Except.Trans
import Optic.Lens
import Optic.Types
import XY
import Data.Either


newtype GameState = GameState
  { player :: Player
  , hand :: Hand
  , enemies :: Array Enemy
  , bestiary :: Bestiary
  , boundaries :: Box
  }

derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where 
  show = genericShow 
instance eqGameState :: Eq GameState where
  eq = genericEq

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
dummyGameState = GameState 
  { player: dummyPlayer
  , hand: [FireBomb, Advance, FireBomb, Advance, FireBomb]
  , enemies: [dummyEnemy]
  , bestiary: dummyBestiary 
  , boundaries: Box {width: 6, height: 6}
  }
