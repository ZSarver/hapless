module Content.Cards where

import Batteries

import Content.XY (XY(..), dummyCoordinate, rectangle, forward)
import Content.Facing (clockwise, widdershins)

data ShortCard 
  = FireBomb 
  | Advance 
  | TurnLeft
  | TurnRight
  | Pass

card :: ShortCard -> Card
card c = case c of
  FireBomb -> Card { effect: [Attack {range: 4, area: (rectangle 3 3)}], cost: 0 }
  Advance -> Card { effect: [AttackMove forward], cost: 2 }
  TurnLeft -> Card { effect: [Rotate widdershins], cost: 2 }
  TurnRight -> Card { effect: [Rotate clockwise], cost: 2 }
  Pass -> Card { effect: [], cost: 0 }
  _ -> dummyAttack


data CardEffect 
  = Attack { range :: Int, area :: Array XY }
  | Move XY
  | AttackMove XY
  | Rotate Int

newtype Card = Card
    { effect :: Array CardEffect
    , cost :: Int
    }

type Hand = Array ShortCard

defaultCost :: Int
defaultCost = 2

dummyAttack :: Card
dummyAttack = Card { effect: [Attack { range: 1, area: [dummyCoordinate] }], cost: attackCost }
    where
        attackCost = defaultCost - 1

-- the dummyCard is a card that has a ridiculously high cost, so that it never actually gets
dummyCard :: Card
dummyCard = Card { effect: [Attack { range: 0, area: [dummyCoordinate] }], cost: 9000 }


derive instance genericShortCard :: Generic ShortCard _
instance showShortCard :: Show ShortCard where show = genericShow
instance encodeShortCard :: Encode ShortCard where encode = genericEncode defaultOptions
instance decodeShortCard :: Decode ShortCard where decode = genericDecode defaultOptions
instance eqShortCard :: Eq ShortCard where eq = genericEq

derive instance newtypeCard :: Newtype Card _
derive instance genericCard :: Generic Card _
instance showCard :: Show Card where show = genericShow
instance encodeCard :: Encode Card where encode = genericEncode defaultOptions
instance decodeCard :: Decode Card where decode = genericDecode defaultOptions
instance eqCard :: Eq Card where eq = genericEq

derive instance genericCardEffect :: Generic CardEffect _
instance showCardEffect :: Show CardEffect where show = genericShow
instance encodeCardEffect :: Encode CardEffect where encode = genericEncode defaultOptions
instance decodeCardEffect :: Decode CardEffect where decode = genericDecode defaultOptions
instance eqCardEffect :: Eq CardEffect where eq = genericEq
