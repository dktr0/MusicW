{-# LANGUAGE CPP #-}
module Sound.MusicW.FloatArraySpec (
  FloatArraySpec(..),
  arraySpecMap,
  arraySpecSize,
  listToArraySpec
) where

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup as Semi
#endif
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

data FloatArraySpec
  = EmptyArray
  | Const Int Double FloatArraySpec
  | Segment [Double] FloatArraySpec
  | Repeated Int [Double] FloatArraySpec
  deriving (Show)

mappendFloatArraySpec :: FloatArraySpec -> FloatArraySpec -> FloatArraySpec
mappendFloatArraySpec EmptyArray x = x
mappendFloatArraySpec x EmptyArray = x
mappendFloatArraySpec (Const i x tl) y = Const i x $ mappendFloatArraySpec tl y
mappendFloatArraySpec (Segment xs tl) y = Segment xs $ mappendFloatArraySpec tl y
mappendFloatArraySpec (Repeated i xs tl) y = Repeated i xs $ mappendFloatArraySpec tl y

#if MIN_VERSION_base(4,9,0)
instance Semi.Semigroup FloatArraySpec where
  (<>) = mappendFloatArraySpec
#endif

instance Monoid FloatArraySpec where
  mempty = EmptyArray
#if MIN_VERSION_base(4,11,0)
#elif MIN_VERSION_base(4,9,0)
  mappend = (Semi.<>)
#else
  mappend = mappendFloatArraySpec
#endif

arraySpecMap :: (Double -> Double) -> FloatArraySpec -> FloatArraySpec
arraySpecMap _ EmptyArray = EmptyArray
arraySpecMap f (Const i x tl)= Const i (f x) $ arraySpecMap f tl
arraySpecMap f (Segment xs tl) = Segment (fmap f xs) $ arraySpecMap f tl
arraySpecMap f (Repeated i xs tl) = Repeated i (fmap f xs) $ arraySpecMap f tl

arraySpecSize :: FloatArraySpec -> Int
arraySpecSize EmptyArray = 0
arraySpecSize (Const i _ tl) = i + arraySpecSize tl
arraySpecSize (Segment xs tl) = (length xs) + arraySpecSize tl
arraySpecSize (Repeated i xs tl) = (i * (length xs)) + arraySpecSize tl

listToArraySpec :: [Double] -> FloatArraySpec
listToArraySpec xs = Segment xs EmptyArray
