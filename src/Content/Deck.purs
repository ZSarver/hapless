module Content.Deck where

import Core.Deck
import Content.Cards(ShortCard(..))

startingDeck ∷ Array ShortCard
startingDeck = [ Back
               , Back
               , Back
               , TurnLeft
               , TurnRight
               , TurnAround
               , TurnAround
               , DoubleAdvance
               , AttackFront
               , AttackFront
               , AttackFront ]
