module Cards where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array(length, drop, (!!), deleteAt)
import Control.Applicative
import Data.Generic (class Generic, gShow)

data CardEffect = Attack | Damage

derive instance genericCardEffect :: Generic CardEffect
instance showCardEffect :: Show CardEffect where
    show = gShow

newtype Card = Card 
    { effect :: Array CardEffect
    , range :: Int
    , area :: { width :: Int, height :: Int}
    , duration :: Int
    , cost :: Int
    }

derive instance genericCard :: Generic Card
instance showCard :: Show Card where
    show = gShow

defaultCost :: Int
defaultCost = 3

fireBomb :: Card
fireBomb = Card { effect: [Damage], range: 4, area: {width: 3, height: 3}, duration: 1, cost: defaultCost }

attack :: Card
attack = Card { effect: [Attack], range: 1, area: {width: 1, height: 1}, duration: 1, cost: attackCost }
    where
        attackCost = defaultCost - 1

dummy :: Card
dummy = Card { effect: [Attack], range: 0, area: {width: 0, height: 0}, duration: 0, cost: 9000}

type Hand = Array Card

discardN :: Int -> Hand -> Maybe Hand
discardN n h = if (length h) < n then Nothing else Just (drop n h)

discard :: Hand -> Maybe Hand
discard = discardN 1

play :: Hand -> Int -> Hand
play h i = fromMaybe h (fromMaybe (pure h) (discardN <$> pure c'.cost <*> h'))
    where 
        (Card c') = fromMaybe dummy (h !! i)
        h' = deleteAt i h