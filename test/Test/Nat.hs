{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Nat where

import Hedgehog
import Test.HUnit (Assertion, assertFailure)

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Nat


unit_fromIntegral0 :: Assertion
unit_fromIntegral0 =
  case Nat.fromIntegral (0 :: Int) of
    Just Zero -> pure ()
    _ -> assertFailure "Not Zero"

unit_fromIntegral1 :: Assertion
unit_fromIntegral1 =
  case Nat.fromIntegral (1 :: Int) of
    Just (Succ Zero) -> pure ()
    _ -> assertFailure "Not One"

unit_fromIntegral2 :: Assertion
unit_fromIntegral2 =
  case Nat.fromIntegral (2 :: Int) of
    Just (Succ (Succ Zero)) -> pure ()
    _ -> assertFailure "Not Two"

hprop_toFrom :: Property
hprop_toFrom = property $ do
  i <- forAll $ G.int (R.linear 0 10000)
  let Just n = Nat.fromIntegral i
  i === Nat.toNum n

nat :: R.Range Int -> Gen Nat
nat r = G.just (Nat.fromIntegral <$> G.int r)

hprop_plus :: Property
hprop_plus = property $ do
  m <- forAll $ nat $ R.linear 0 10000
  n <- forAll $ nat $ R.linear 0 10000
  Nat.toNum m + Nat.toNum n === (Nat.toNum (m `Nat.plus` n) :: Integer)

hprop_minus :: Property
hprop_minus = property $ do
  m <- forAll $ nat $ R.linear 0 10000
  n <- forAll $ nat $ R.linear 0 10000
  let res = max 0 $ Nat.toNum m - Nat.toNum n :: Integer
  res === (Nat.toNum (m `Nat.minus` n) :: Integer)
