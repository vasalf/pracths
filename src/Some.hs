module Some where

import Type.Reflection ((:~~:) (HRefl), Typeable, eqTypeRep, typeOf, typeRep, SomeTypeRep(..))
import qualified Data.Map.Strict as Map

data Some' = forall a. MkSome' a

data Some c where
  MkSome :: c a => a -> Some c


showSome :: Some Show -> String
showSome (MkSome x) = show x


type ShowAndNum t = (Show t, Num t)

class ShowAndNum t => ShowAndNumClass t
instance ShowAndNum t => ShowAndNumClass t

showableList :: [Some ShowAndNumClass]
showableList = [MkSome (1 :: Int), MkSome (2 :: Integer)]


data Variant where
  MkVariant :: Typeable a => a -> Variant

showVariantType :: Variant -> String
showVariantType (MkVariant x) = show (typeOf x)

fromVariant :: forall b. Typeable b => Variant -> Maybe b
fromVariant (MkVariant x) =
  case typeOf x `eqTypeRep` typeRep @b of
    Just HRefl -> Just x
    Nothing -> Nothing
--fmap (\HRefl -> x) (typeOf x `eqTypeRep` typeRep @b)

{-
data (:~~:) (a :: Type) (b :: Type) where
  HRefl :: c :~~: c
-}

data OrdVariant where
  MkOrdVariant :: (Ord a, Typeable a) => a -> OrdVariant

instance Eq OrdVariant where
  MkOrdVariant x == MkOrdVariant y =
    case typeOf x `eqTypeRep` typeOf y of
      Just HRefl -> x == y
      Nothing    -> False

instance Ord OrdVariant where
  MkOrdVariant x <= MkOrdVariant y =
    let tx = typeOf x
        ty = typeOf y
     in case tx `eqTypeRep` ty of
          Just HRefl -> x <= y
          Nothing    -> SomeTypeRep tx <= SomeTypeRep ty

type PolyMap = Map.Map OrdVariant Variant

empty :: PolyMap
empty = Map.empty

insert :: (Ord a, Typeable a) => a -> a -> PolyMap -> PolyMap
insert x y = Map.insert (MkOrdVariant x) (MkVariant y)

lookup :: (Ord a, Typeable a) => a -> PolyMap -> Maybe a
lookup x m = case MkOrdVariant x `Map.lookup` m of
               Nothing            -> Nothing
               Just (MkVariant y) -> case typeOf x `eqTypeRep` typeOf y of
                                       Just HRefl -> Just y
                                       {- unreachable -}
                                       Nothing    -> Nothing 
