module CardData where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (genericEncode, defaultOptions)
import Data.Generic.Rep.Eq (genericEq)
import Box


newtype Card = Card
    { effect :: Array CardEffect
    , range :: Int
    , area :: Box
    , duration :: Int
    , cost :: Int
    }

derive instance genericCard :: Generic Card _
instance showCard :: Show Card where show = genericShow
instance encodeCard :: Encode Card where encode = genericEncode defaultOptions
instance eqCard :: Eq Card where eq = genericEq


type Hand = Array Card

data CardEffect = Attack | Move

derive instance genericCardEffect :: Generic CardEffect _
instance showCardEffect :: Show CardEffect where show = genericShow
instance encodeCardEffect :: Encode CardEffect where encode = genericEncode defaultOptions
instance eqCardEffect :: Eq CardEffect where eq = genericEq

defaultCost :: Int
defaultCost = 3

fireBomb :: Card
fireBomb = Card { effect: [Attack], range: 4, area: Box {width: 3, height: 3}, duration: 1, cost: defaultCost }

dummyAttack :: Card
dummyAttack = Card { effect: [Attack], range: 1, area: Box {width: 1, height: 1}, duration: 1, cost: attackCost }
    where
        attackCost = defaultCost - 1

forward1 :: Card
forward1 = Card { effect: [Move], range: 1, area: Box {width: 0, height: 0}, duration: 1, cost: moveCost }
    where
        moveCost = defaultCost + 1

-- the dummyCard is a card that has a ridiculously high cost, so that it never actually gets
dummyCard :: Card
dummyCard = Card { effect: [Attack], range: 0, area: Box {width: 0, height: 0}, duration: 0, cost: 9000}
