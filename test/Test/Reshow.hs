module Test.Reshow where

import Hedgehog
import Test.HUnit (Assertion, (@=?))

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Reshow

reshowInt :: (String -> Maybe String) -> Property
reshowInt f = property $ do
  v <- forAll $ G.int R.linearBounded
  Just (show v) === f (show v)

reshowDouble :: (String -> Maybe String) -> Property
reshowDouble f = property $ do
  v <- forAll $ G.int $ R.linear (-100) 100 
  Just (show v <> ".0") === f (show v)

data Proxy a = Proxy

hprop_reshowAs_Int :: Property
hprop_reshowAs_Int = reshowInt (reshowAs @Int)

hprop_reshowAs'_Int :: Property
hprop_reshowAs'_Int = reshowInt (reshowAs' (Proxy :: Proxy Int))

hprop_reshowAs_Double :: Property
hprop_reshowAs_Double = reshowDouble (reshowAs @Double)

hprop_reshowAs'_Double :: Property
hprop_reshowAs'_Double = reshowDouble (reshowAs' (Proxy :: Proxy Double))

unit_reshowAs_Nothing :: Assertion
unit_reshowAs_Nothing =
  Nothing @=? reshowAs @Int "abc"

unit_reshowAs'_Nothing :: Assertion
unit_reshowAs'_Nothing =
  Nothing @=? reshowAs' (Proxy :: Proxy Int) "abc"
