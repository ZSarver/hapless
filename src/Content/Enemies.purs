module Content.Enemies where

import Batteries 

import Content.Facing
import Content.XY
import Data.Generic.Rep.Ord (genericCompare)

data Species = Skeleton | Ghost | Slime

derive instance genericSpecies :: Generic Species _
instance showSpecies :: Show Species where show = genericShow
instance encodeSpecies :: Encode Species where encode = genericEncode defaultOptions
instance decodeSpecies :: Decode Species where decode = genericDecode defaultOptions
instance eqSpecies :: Eq Species where eq = genericEq
instance ordSpecies :: Ord Species where compare = genericCompare

data MoveBehavior = Righty | Steadfast | Waffly

derive instance genericMoveBehavior :: Generic MoveBehavior _
instance showMoveBehavior :: Show MoveBehavior where show = genericShow
instance encodeMoveBehavior :: Encode MoveBehavior where encode = genericEncode defaultOptions
instance decodeMoveBehavior :: Decode MoveBehavior where decode = genericDecode defaultOptions
instance eqMoveBehavior :: Eq MoveBehavior where eq = genericEq

data AttackBehavior = Smash | Rend | Befuddle

derive instance genericAttackBehavior :: Generic AttackBehavior _
instance showAttackBehavior :: Show AttackBehavior where show = genericShow
instance encodeAttackBehavior :: Encode AttackBehavior where encode = genericEncode defaultOptions
instance decodeAttackBehavior :: Decode AttackBehavior where decode = genericDecode defaultOptions
instance eqAttackBehavior :: Eq AttackBehavior where eq = genericEq

newtype Enemy = Enemy
    { species :: Species
    , location :: XY
    , facing :: Facing
    --    , moveBehavior :: MoveBehavior       instead of keeping these here, we can put them in a lookup (Species -> Behavior)
    --    , attackBehavior :: AttackBehavior       that lives higher up in the game state
    }

derive instance newtypeEnemy :: Newtype Enemy _
derive instance genericEnemy :: Generic Enemy _
instance showEnemy :: Show Enemy where show = genericShow
instance encodeEnemy :: Encode Enemy where encode = genericEncode defaultOptions
instance decodeEnemy :: Decode Enemy where decode = genericDecode defaultOptions
instance eqEnemy :: Eq Enemy where eq = genericEq

type Gaggle = Array Enemy

-- a dummy enemy for testing purposes
dummyEnemy :: Enemy
dummyEnemy = Enemy { species: Skeleton, location: XY {x: 6, y: 2}, facing: South}

dummyEnemy2 = Enemy { species: Ghost, location: XY {x: 1, y: 2}, facing: West }
