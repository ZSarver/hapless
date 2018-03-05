module CardData where

import Prelude
import Data.Generic (class Generic, gShow, gEq)

data CardEffect = Attack | Move

newtype Card = Card 
    { effect :: Array CardEffect
    , range :: Int
    , area :: { width :: Int, height :: Int}
    , duration :: Int
    , cost :: Int
    }

type Hand = Array Card

newtype Coordinate = Coordinate {x :: Int, y :: Int}

derive instance genericCard :: Generic Card
instance showCard :: Show Card where
    show = gShow

derive instance genericCardEffect :: Generic CardEffect
instance showCardEffect :: Show CardEffect where
    show = gShow
instance eqCardEffect :: Eq CardEffect where
    eq = gEq

derive instance genericCoordinate :: Generic Coordinate
instance eqCoordinate :: Eq Coordinate where
    eq = gEq

defaultCost :: Int
defaultCost = 3

fireBomb :: Card
fireBomb = Card { effect: [Attack], range: 4, area: {width: 3, height: 3}, duration: 1, cost: defaultCost }

dummyAttack :: Card
dummyAttack = Card { effect: [Attack], range: 1, area: {width: 1, height: 1}, duration: 1, cost: attackCost }
    where
        attackCost = defaultCost - 1

forward1 :: Card
forward1 = Card { effect: [Move], range: 1, area: {width: 0, height: 0}, duration: 1, cost: moveCost }
    where
        moveCost = defaultCost + 1

-- the dummyCard is a card that has a ridiculously high cost, so that it never actually gets
dummyCard :: Card
dummyCard = Card { effect: [Attack], range: 0, area: {width: 0, height: 0}, duration: 0, cost: 9000}
