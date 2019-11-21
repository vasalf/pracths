module HList where

import Data.Kind (Constraint, Type)


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

type family All (c :: Type -> Constraint) (as :: [Type]) :: Constraint where
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
