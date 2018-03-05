module EnemyData where

import Data.Generic (class Generic)
import Facing

data Species = Skeleton | Ghost | Slime
derive instance genericSpecies :: Generic Species

data MoveBehavior = Whatever
derive instance genericMoveBehavior :: Generic MoveBehavior

data AttackBehavior = Smash | Rend | Befuddle
derive instance genericAttackBehavior :: Generic AttackBehavior

newtype Enemy = Enemy 
    { species :: Species
    , location :: {x :: Int, y :: Int}
    , moveBehavior :: MoveBehavior
    , attackBehavior :: AttackBehavior
    , facing :: Facing
    }

derive instance genericEnemy :: Generic Enemy

type Gaggle = Array Enemy

-- a dummy enemy for testing purposes
dummyEnemy :: Enemy
dummyEnemy = Enemy { species: Skeleton, location: {x: 6, y: 2}, moveBehavior: Whatever, attackBehavior: Smash, facing: South}

dummyEnemy2 = Enemy { species: Slime, location: {x: 1, y: 2}, moveBehavior: Whatever, attackBehavior: Rend, facing: West }
