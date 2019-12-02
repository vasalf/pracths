module Test.Vec where

import Hedgehog
import Test.HUnit

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Nat
import Vec


unit_vecShow :: Assertion
unit_vecShow =
  "Vec{3}[1,2,3]" @=? show (VCons 1 $ VCons 2 $ VCons (3 :: Int) $ VNil)

type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two
type Four = 'Succ Three
type Five = 'Succ Four

hprop_vtake3of4 :: Property
hprop_vtake3of4 = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  let v = VCons a $ VCons b $ VCons c $ VCons d $ VNil
  "Vec{3}[" <> show a <> "," <> show b <> "," <> show c <> "]" === show (vtake @Three v)

unit_vtailEmpty :: Assertion
unit_vtailEmpty =
  VNil @=? (VNil :: Vec 'Zero Int)

hprop_vtailNotEmpty :: Property
hprop_vtailNotEmpty = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  let expected = VCons b $ VCons c $ VCons (d :: Int) $ VNil
  let toTest = VCons a expected
  expected === vtail toTest

-- Compiles <=> passes
unit_testTwoMinusTwo :: Assertion
unit_testTwoMinusTwo = do
  let (v :: Vec 'Zero Int) = VNil
  let (u :: Vec (Minus Two Two) Int) = v
  v @=? u

-- Compiles <=> passes
unit_testFourMinusTwo :: Assertion
unit_testFourMinusTwo = do
  let v = VCons 1 $ VCons 2 $ VCons 3 $ VCons (4 :: Int) $ VNil
  let (u :: Vec (Minus Four Two) Int) = vtake @Two v
  (VCons 1 $ VCons 2 $ VNil) @=? u

unit_vec2listEmpty :: Assertion
unit_vec2listEmpty =
  [] @=? vec2list (VNil :: Vec 'Zero ())

hprop_vec2listNonEmpty :: Property
hprop_vec2listNonEmpty = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  let v = VCons a $ VCons b $ VCons c $ VCons d VNil
  [a, b, c, d] === vec2list v

unit_list2vecZeroEmpty :: Assertion
unit_list2vecZeroEmpty =
  Just VNil @=? list2vec @'Zero ([] :: [Int])
 
unit_list2vecZeroNotEmpty :: Assertion
unit_list2vecZeroNotEmpty =
  Nothing @=? list2vec @'Zero [(1 :: Int), 7, 9]

unit_list2vecThreeEmpty :: Assertion
unit_list2vecThreeEmpty =
  Nothing @=? list2vec @Three ([] :: [Int])

hprop_list2vecThreeThree :: Property
hprop_list2vecThreeThree = property $ do
  let genint = G.int R.linearBounded
  (a, b, c) <- forAll $ (,,) <$> genint <*> genint <*> genint
  let v = VCons a $ VCons b $ VCons c VNil
  Just v === list2vec @Three [a, b, c]

hprop_list2vecThreeFour :: Property
hprop_list2vecThreeFour = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  Nothing === list2vec @Three [a, b, c, d]
