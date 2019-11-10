module Test.TypeLitsVec where

import Hedgehog
import Test.HUnit

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import TypeLitsVec


unit_vecShow :: Assertion
unit_vecShow =
  "Vec{3}[1,2,3]" @=? show (VCons 1 $ VCons 2 $ VCons (3 :: Int) $ VNil)

hprop_vappend :: Property
hprop_vappend = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  let v = VCons a $ VCons b $ VCons c $ VCons d VNil
  let first = VCons a $ VCons b VNil
  let second = VCons c $ VCons d VNil
  v === first `vappend` second

hprop_vappendEmpty :: Property
hprop_vappendEmpty = property $ do
  let genint = G.int R.linearBounded
  (a, b) <- forAll $ (,) <$> genint <*> genint
  let v = VCons a $ VCons b VNil
  v === VNil `vappend` v
  v === v `vappend` VNil

hprop_vtake3of4 :: Property
hprop_vtake3of4 = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  let v = VCons a $ VCons b $ VCons c $ VCons d VNil
  (VCons a $ VCons b $ VCons c VNil) === vtake @3 v

hprop_vtail :: Property
hprop_vtail = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  let expected = VCons b $ VCons c $ VCons (d :: Int) VNil
  let toTest = VCons a expected
  expected === vtail toTest
