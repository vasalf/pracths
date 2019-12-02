module HList where

import Data.Kind (Constraint, Type)

import Nat
import Vec


data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

foo :: HList '[Int, String, Bool]
foo = HCons 10 $ HCons "hello" $ HCons True $ HNil

hlength :: Num n => HList as -> n
hlength = go 0
  where
    go :: Num n => n -> HList bs -> n
    go a HNil = a
    go a (HCons _ xs) = go (a + 1) xs

type family All (c :: k -> Constraint) (as :: [k]) :: Constraint where
  All _ '[] = ()
  All c (a ': as) = (c a, All c as)

instance All Show xs => Show (HList xs) where
  show xs = "[" <> go xs
    where
      go :: All Show ys => HList ys -> String
      go HNil = "]"
      go (HCons y HNil) = show y<>"]"
      go (HCons y ys) = show y<>","<>go ys

instance All Eq xs => Eq (HList xs) where
  HNil       == HNil       = True
  HCons x xs == HCons y ys = (x == y) && (xs == ys)

type ShowAndNum t = (Show t, Num t)

showZeroAs :: forall t. ShowAndNum t => String
showZeroAs = show (0 :: t)

hhead :: HList (a ': as) -> a
hhead (HCons x _) = x

htail :: HList (a ': as) -> HList as
htail (HCons _ xs) = xs

type family ReverseGo (xs :: [k]) (ys :: [k]) :: [k] where
  ReverseGo (x ': xs) ys = ReverseGo xs (x ': ys)
  ReverseGo '[] ys = ys

type family Reverse (xs :: [k]) where
  Reverse xs = ReverseGo xs '[]

hreverse :: HList as -> HList (Reverse as)
hreverse = (flip go) HNil
  where
    go :: forall as bs. HList as -> HList bs -> HList (ReverseGo as bs)
    go (HCons x xs) = go xs . HCons x
    go HNil = id

type family Replicate (n :: Nat) (a :: k) :: [k] where
  Replicate 'Zero _ = '[]
  Replicate ('Succ n) a = a ': Replicate n a

vec2hlist :: Vec n a -> HList (Replicate n a)
vec2hlist VNil = HNil
vec2hlist (VCons x xs) = HCons x (vec2hlist xs)


type family Len (as :: [k]) :: Nat where
  Len '[] = 'Zero
  Len (x ': xs) = 'Succ (Len xs)

hlist2vec :: All ((~) a) as => HList as -> Vec (Len as) a
hlist2vec HNil = VNil
hlist2vec (HCons x xs) = VCons x (hlist2vec xs)

class HList2Vec' (n :: Nat) where
  hlist2vec' :: HList (Replicate n a) -> Vec n a

instance HList2Vec' 'Zero where
  hlist2vec' HNil = VNil

instance HList2Vec' n => HList2Vec' ('Succ n) where
  hlist2vec' (HCons x xs) = VCons x (hlist2vec' xs)


type family Map (f :: Type) (as :: [k1]) :: [k2] where
  Map _ '[] = '[]
  Map f (a ': as) = MapType f a ': Map f as


data Shower

class MapFunction (f :: Type) (a :: Type) where
  type MapType f a :: Type
  mapVal :: a -> MapType f a

instance MapFunction Shower Int where
  type MapType Shower Int = String
  mapVal = show

instance MapFunction Shower String where
  type MapType Shower String = String
  mapVal = id

instance MapFunction Shower Bool where
  type MapType Shower Bool = Char
  mapVal False = 'N'
  mapVal True = 'Y'

class HMap (f :: Type) (as :: [Type]) where
  hmap :: HList as -> HList (Map f as)

instance HMap f '[] where
  hmap HNil = HNil

instance (MapFunction f a, HMap f as) => HMap f (a ': as) where
  hmap (HCons x xs) = HCons (mapVal @f x) (hmap @f xs)

class FoldMapTo (f :: Type) where
  type FoldMapType f :: Type

class FoldMapFunction (f :: Type) (a :: Type) where
  foldMapVal :: a -> FoldMapType f

class HFoldMap (f :: Type) (as :: [Type]) where
  hfoldMap :: Monoid (FoldMapType f) => HList as -> FoldMapType f

instance HFoldMap f '[] where
  hfoldMap HNil = mempty

instance (FoldMapFunction f a, HFoldMap f as) => HFoldMap f (a ': as) where
  hfoldMap (HCons x xs) = foldMapVal @f x <> hfoldMap @f xs

newtype HListShower = HListShower { unHListShower :: Maybe String }

instance Semigroup HListShower where
  HListShower (Just l) <> HListShower (Just r) = HListShower $ Just $ l <> "," <> r
  HListShower Nothing  <> r                    = r
  l                    <> _                    = l

instance Monoid HListShower where
  mempty = HListShower Nothing

instance Show HListShower where
  show (HListShower (Just s)) = "[" <> s <> "]"
  show (HListShower Nothing)  = "[]"

instance FoldMapTo Shower where
  type FoldMapType Shower = HListShower

instance (MapFunction Shower a, String ~ MapType Shower a) => FoldMapFunction Shower a where
  foldMapVal = HListShower . Just . mapVal @Shower

class List2hlist (n :: Nat) where
  list2hlist :: forall a. [a] -> Maybe (HList (Replicate n a))

instance List2hlist 'Zero where
  list2hlist [] = Just HNil
  list2hlist _  = Nothing

instance List2hlist n => List2hlist ('Succ n) where
  list2hlist []     = Nothing
  list2hlist (x:xs) = HCons x <$> list2hlist @n xs
