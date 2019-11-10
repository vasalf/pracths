module Nat where


-- data Nat = Zero | Succ Nat

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

deriving instance Eq Nat

plus :: Nat -> Nat -> Nat
plus = go id
  where
    go f Zero n = f n
    go f (Succ m) n = go (Succ . f) m n

minus :: Nat -> Nat -> Nat
minus x Zero = x
minus Zero (Succ _) = Zero
minus (Succ m) (Succ n) = minus m n

toNum :: Num n => Nat -> n
toNum = go 0
  where
    go a Zero = a
    go a (Succ m) = go (a + 1) m

fromIntegral :: Integral i => i -> Maybe Nat
fromIntegral i
  | i < 0 = Nothing
  | otherwise = Just $ go Zero i
    where
      go a 0 = a
      go a k = go (Succ a) (k - 1)

instance Show Nat where
  show n = "Nat(" <> show (toNum n :: Integer) <> ")"
