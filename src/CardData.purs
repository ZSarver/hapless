module CardData where

import Prelude
import Data.Generic (class Generic, gShow, gEq)
import Coordinate

data CardEffect = Attack | Move

type Card = 
    { effect :: Array CardEffect
    , range :: Int
    , duration :: Int
    , cost :: Int
    }

type Hand = Array Card

derive instance genericCardEffect :: Generic CardEffect
instance showCardEffect :: Show CardEffect where
    show = gShow
instance eqCardEffect :: Eq CardEffect where
    eq = gEq

defaultCost :: Int
defaultCost = 3

fireBomb :: Card
fireBomb = { effect: [Attack], range: 4, area: (rectangle 3 3), duration: 1, cost: defaultCost }

dummyAttack :: Card
dummyAttack = { effect: [Attack], range: 1, area: [dummyCoordinate], duration: 1, cost: attackCost }
    where
        attackCost = defaultCost - 1

forward1 :: Card
forward1 = { effect: [Move], range: 1, area: [dummyCoordinate], duration: 1, cost: moveCost }
    where
        moveCost = defaultCost + 1

-- the dummyCard is a card that has a ridiculously high cost, so that it never actually gets
dummyCard :: Card
dummyCard = { effect: [Attack], range: 0, area: [dummyCoordinate], duration: 0, cost: 9000}
