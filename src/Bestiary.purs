module Bestiary where
  
import Batteries

import Content.Enemies
import Data.Map
import Data.Tuple
import Data.Foreign.Class

type Entry = Tuple AttackBehavior MoveBehavior
newtype Bestiary = Bestiary (Map Species Entry)

toRs :: Bestiary -> Array R
toRs (Bestiary m) = map f (toAscUnfoldable m)
  where
    f (Tuple s (Tuple a m)) = R s a m

fromRs :: Array R -> Bestiary
fromRs rs = Bestiary ( fromFoldable $ map f rs)
  where
    f (R s a m) = Tuple s (Tuple a m)

instance showBestiary :: Show Bestiary where show b = show (toRs b)
instance encodeBestiary :: Encode Bestiary where encode b = encode (toRs b)
instance decodeBestiary :: Decode Bestiary where decode x = map (fromRs) (decode x)

dummyBestiary :: Bestiary
dummyBestiary = fromRs 
  [ R Skeleton Rend Whatever
  , R Ghost Befuddle Whatever
  , R Slime Smash Whatever 
  ]



data R = R Species AttackBehavior MoveBehavior

derive instance genericR :: Generic R _
instance showR :: Show R where show = genericShow
instance encodeR :: Encode R where  encode = genericEncode defaultOptions
instance decodeR :: Decode R where decode = genericDecode defaultOptions
instance eqR :: Eq R where eq = genericEq


