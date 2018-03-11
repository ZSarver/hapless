module Core.Deck where
  
import Batteries

import Content.Cards(ShortCard)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foreign.Class

data Deck = Deck (Array ShortCard) (Array ShortCard)

drawPile :: Deck -> Array ShortCard
drawPile (Deck a b) = a

discardPile :: Deck -> Array ShortCard
discardPile (Deck a b) = b

derive instance genericDeck :: Generic Deck _
instance showDeck :: Show Deck where show = genericShow
instance eqDeck :: Eq Deck where eq = genericEq
instance encodeDeck :: Encode Deck where encode = genericEncode defaultOptions
instance decodeDeck :: Decode Deck where decode = genericDecode defaultOptions
