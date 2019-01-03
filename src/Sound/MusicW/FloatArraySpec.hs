module Sound.MusicW.FloatArraySpec where

data FloatArraySpec
  = EmptyArray
  | Const Int Double FloatArraySpec
  | Segment [Double] FloatArraySpec
  | Repeated Int [Double] FloatArraySpec
  deriving (Show)

instance Monoid FloatArraySpec where
  mempty = EmptyArray
  mappend EmptyArray x = x
  mappend x EmptyArray = x
  mappend (Const i x tl) y = Const i x $ mappend tl y
  mappend (Segment xs tl) y = Segment xs $ mappend tl y
  mappend (Repeated i xs tl) y = Repeated i xs $ mappend tl y

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
