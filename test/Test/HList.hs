module Test.HList where

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Test.HUnit (Assertion, (@=?))

import HList


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
