module EnemyData where

data Species = Skeleton | Ghost | Slime
data MoveBehavior = Whatever
data AttackBehavior = Smash | Rend | Befuddle

newtype Enemy = Enemy 
    { species :: Species
    , location :: {x :: Int, y :: Int}
    , moveBehavior :: MoveBehavior
    , attackBehavior :: AttackBehavior
    }

type Gaggle = Array Enemy

-- a dummy enemy for testing purposes
dummyEnemy :: Enemy
dummyEnemy = Enemy { species: Skeleton, location: {x: 1, y: 1}, moveBehavior: Whatever, attackBehavior: Smash }