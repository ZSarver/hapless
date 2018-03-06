module EnemyData where

import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (genericEncode, defaultOptions)
import Facing
import XY

data Species = Skeleton | Ghost | Slime
derive instance genericSpecies :: Generic Species _
instance encodeSpecies :: Encode Species where encode = genericEncode defaultOptions
instance showSpecies :: Show Species where show = genericShow

data MoveBehavior = Whatever
derive instance genericMoveBehavior :: Generic MoveBehavior _
instance encodeMoveBehvior :: Encode MoveBehavior where encode = genericEncode defaultOptions
instance showMoveBehavior :: Show MoveBehavior where show = genericShow

data AttackBehavior = Smash | Rend | Befuddle
derive instance genericAttackBehavior :: Generic AttackBehavior _
instance encodeAttackBehvior :: Encode AttackBehavior where encode = genericEncode defaultOptions
instance showAttackBehavior :: Show AttackBehavior where show = genericShow

newtype Enemy = Enemy
    { species :: Species
    , location :: XY
    , moveBehavior :: MoveBehavior
    , attackBehavior :: AttackBehavior
    , facing :: Facing
    }

derive instance genericEnemy :: Generic Enemy _
instance encodeEnemy :: Encode Enemy where encode = genericEncode defaultOptions
instance showEnemy :: Show Enemy where show = genericShow

type Gaggle = Array Enemy

-- a dummy enemy for testing purposes
dummyEnemy :: Enemy
dummyEnemy = Enemy { species: Skeleton, location: XY {x: 6, y: 2}, moveBehavior: Whatever, attackBehavior: Smash, facing: South}

dummyEnemy2 = Enemy { species: Slime, location: XY {x: 1, y: 2}, moveBehavior: Whatever, attackBehavior: Rend, facing: West }
