module Core.Bestiary where
  
import Batteries

import Content.Enemies
import Data.Map as M
import Data.Tuple
import Data.Foreign.Class

import Partial.Unsafe
import Data.Foreign
import Control.Monad.Except
import Data.Either

import Partial (crash)

type Entry = Tuple AttackBehavior MoveBehavior
newtype Bestiary = Bestiary (M.Map Species Entry)


attackBehavior :: Bestiary -> Species -> AttackBehavior
attackBehavior (Bestiary b) s = fst $ unsafePartial $ fromJust $ M.lookup s b

moveBehavior :: Bestiary -> Species -> MoveBehavior
moveBehavior (Bestiary b) s = snd $ unsafePartial $ fromJust $ M.lookup s b

toRs :: Bestiary -> Array R
toRs (Bestiary m) = map f (M.toAscUnfoldable m)
  where
    f (Tuple s (Tuple a m)) = R s a m

fromRs :: Array R -> F Bestiary
fromRs rs = pure $ Bestiary ( M.fromFoldable $ map f rs)
  where
    f (R s a m) = Tuple s (Tuple a m)

instance showBestiary :: Show Bestiary where show b = show (toRs b)
instance encodeBestiary :: Encode Bestiary where encode b = encode (toRs b)
instance decodeBestiary :: Decode Bestiary where decode x = decode x >>= fromRs
instance eqBesiary :: Eq Bestiary where eq x y = (toRs x) == (toRs y)

dummyBestiary :: Bestiary
dummyBestiary =  case runExcept b of
  Left _ -> unsafePartial crash
  Right a -> a
  where 
  b = fromRs 
    [ R Skeleton Rend Steadfast
    , R Ghost Befuddle Waffly
    , R Slime Smash Righty
    ]



data R = R Species AttackBehavior MoveBehavior

derive instance genericR :: Generic R _
instance showR :: Show R where show = genericShow
instance encodeR :: Encode R where  encode = genericEncode defaultOptions
instance decodeR :: Decode R where decode = genericDecode defaultOptions
instance eqR :: Eq R where eq = genericEq


