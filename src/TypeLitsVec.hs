{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module TypeLitsVec where

import Data.Kind (Type)
import Data.Singletons (sing)
import GHC.TypeLits

data Vec (n :: Nat) (a :: Type) where
  VNil :: Vec 0 a
  VCons :: a -> Vec m a -> Vec (m + 1) a

deriving instance Eq a => Eq (Vec n a)

-- The warning on this line is most probably a GHC bug: the constraint allows
-- us not to match the first constructor. If we try to, a warning of
-- inaccessible code appears.
vhead :: (1 <= n) => Vec n a -> a
vhead (VCons x _) = x

vappend :: Vec m a -> Vec n a -> Vec (m + n) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (xs `vappend` ys)

instance (KnownNat n, Show a) => Show (Vec n a) where
  show v = "Vec{" <> show (natVal @n sing) <> "}[" <> go v
    where
      go :: Vec m a -> String
      go VNil = "]"
      go (VCons x VNil) = show x <> "]"
      go (VCons x xs) = show x <> "," <> go xs

unvcons :: (1 <= n) => Vec n a -> (a, Vec (n - 1) a)
unvcons (VCons x xs) = (x, xs)

class Vtake (n :: Nat) (m :: Nat) where
  vtake :: (n <= m) => Vec m a -> Vec n a

instance {-# OVERLAPPING #-} Vtake 0 m where
  vtake _ = VNil

instance (1 <= n, (n - 1) <= (m - 1), Vtake (n - 1) (m - 1)) => Vtake n m where
  vtake v = VCons x (vtake @(n - 1) xs)
    where
      (x, xs) = unvcons v

vtail :: (1 <= n) => Vec n a -> Vec (n - 1) a
vtail (VCons _ xs) = xs
