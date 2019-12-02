module Test.HList where

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Test.HUnit (Assertion, (@=?))

import HList
import Nat


hprop_show :: Property
hprop_show = property $ do
  a <- forAll $ G.int R.linearBounded
  b <- forAll $ G.string (R.linear 0 100) G.alphaNum
  c <- forAll $ G.bool

  let hl = HCons a $ HCons b $ HCons c $ HNil
  "["<>show a<>","<>show b<>","<>show c<>"]" === show hl

hprop_reverse :: Property
hprop_reverse = property $ do
  a <- forAll $ G.int R.linearBounded
  b <- forAll $ G.string (R.linear 0 100) G.alphaNum
  c <- forAll $ G.bool

  let hl = HCons a $ HCons b $ HCons c HNil
  let expected = HCons c $ HCons b $ HCons a HNil
  hreverse hl === expected


unit_reverseEmpty :: Assertion
unit_reverseEmpty =
  HNil @=? hreverse HNil


unit_reverseSingleton :: Assertion
unit_reverseSingleton = do
  let xs = HCons (179 :: Int) HNil
  xs @=? hreverse xs

hprop_hmap :: Property
hprop_hmap = property $ do
  a <- forAll $ G.int R.linearBounded
  b <- forAll $ G.string (R.linear 0 20) G.alphaNum
  c <- forAll G.bool

  let hl = HCons a $ HCons b $ HCons c $ HNil
  let yesNo x = if x then "Y" else "N"
  "[\""<>show a<>"\",\""<>b<>"\",'"<>yesNo c<>"']" === show (hmap @Shower hl)

hprop_hfoldMap :: Property
hprop_hfoldMap = property $ do
  a <- forAll $ G.int R.linearBounded
  b <- forAll $ G.string (R.linear 0 20) G.alphaNum

  let hl = HCons a $ HCons b HNil
  "[" <> show a <> "," <> b <> "]" === show (hfoldMap @Shower hl)

unit_list2hlistZeroEmpty :: Assertion
unit_list2hlistZeroEmpty =
  Just HNil @=? list2hlist @'Zero ([] :: [Int])
 
unit_list2hlistZeroNotEmpty :: Assertion
unit_list2hlistZeroNotEmpty =
  Nothing @=? list2hlist @'Zero [(1 :: Int), 7, 9]

type Three = 'Succ ('Succ ('Succ 'Zero))

unit_list2hlistThreeEmpty :: Assertion
unit_list2hlistThreeEmpty =
  Nothing @=? list2hlist @Three ([] :: [Int])

hprop_list2hlistThreeThree :: Property
hprop_list2hlistThreeThree = property $ do
  let genint = G.int R.linearBounded
  (a, b, c) <- forAll $ (,,) <$> genint <*> genint <*> genint
  let v = HCons a $ HCons b $ HCons c HNil
  Just v === list2hlist @Three [a, b, c]

hprop_list2hlistThreeFour :: Property
hprop_list2hlistThreeFour = property $ do
  let genint = G.int R.linearBounded
  (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
  Nothing === list2hlist @Three [a, b, c, d]
