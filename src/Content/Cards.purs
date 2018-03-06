module Content.Cards where

import Batteries

import XY (XY, dummyCoordinate, rectangle)

-- to make serialized game state smaller
data ShortCard = FireBomb | Advance | Punch

derive instance genericShortCard :: Generic ShortCard _
instance showShortCard :: Show ShortCard where show = genericShow
instance encodeShortCard :: Encode ShortCard where encode = genericEncode defaultOptions
instance decodeShortCard :: Decode ShortCard where decode = genericDecode defaultOptions
instance eqShortCard :: Eq ShortCard where eq = genericEq

card :: ShortCard -> Card
card c = case c of
  FireBomb -> fireBomb
  Advance -> forward1
  Punch -> dummyAttack


newtype Card = Card
    { effect :: Array CardEffect
    , range :: Int
    , area :: Array XY
    , duration :: Int
    , cost :: Int
    }


derive instance newtypeCard :: Newtype Card _
derive instance genericCard :: Generic Card _
instance showCard :: Show Card where show = genericShow
instance encodeCard :: Encode Card where encode = genericEncode defaultOptions
instance decodeCard :: Decode Card where decode = genericDecode defaultOptions
instance eqCard :: Eq Card where eq = genericEq

type Hand = Array ShortCard

data CardEffect = Attack | Move

derive instance genericCardEffect :: Generic CardEffect _
instance showCardEffect :: Show CardEffect where show = genericShow
instance encodeCardEffect :: Encode CardEffect where encode = genericEncode defaultOptions
instance decodeCardEffect :: Decode CardEffect where decode = genericDecode defaultOptions
instance eqCardEffect :: Eq CardEffect where eq = genericEq

defaultCost :: Int
defaultCost = 3

fireBomb :: Card
fireBomb = Card { effect: [Attack], range: 4, area: (rectangle 3 3), duration: 1, cost: defaultCost }

dummyAttack :: Card
dummyAttack = Card { effect: [Attack], range: 1, area: [dummyCoordinate], duration: 1, cost: attackCost }
    where
        attackCost = defaultCost - 1

forward1 :: Card
forward1 = Card { effect: [Move], range: 1, area: [dummyCoordinate], duration: 1, cost: moveCost }
    where
        moveCost = defaultCost + 1

-- the dummyCard is a card that has a ridiculously high cost, so that it never actually gets
dummyCard :: Card
dummyCard = Card { effect: [Attack], range: 0, area: [dummyCoordinate], duration: 0, cost: 9000}
