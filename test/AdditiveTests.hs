{-# LANGUAGE FlexibleInstances #-}

module AdditiveTests (tests) where

import Algebra.Additive (propAssociative, propCommutative, propIdentity, propInverse)
import Dual (Dual (Dual))
import Helpers (approxRat)
import NumericPrelude
import Test.QuickCheck (Arbitrary (arbitrary), quickCheckAll)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC (testProperty)
import TestInstances ()
import Prelude ((<$>))

{- Additive Integer Properties -}

prop_additive_associativity_integer :: Dual Integer -> Dual Integer -> Dual Integer -> Bool
prop_additive_associativity_integer = propAssociative

prop_additive_commutative_integer :: Dual Integer -> Dual Integer -> Bool
prop_additive_commutative_integer = propCommutative

prop_additive_identity_integer :: Dual Integer -> Bool
prop_additive_identity_integer = propIdentity

prop_additive_inverse_integer :: Dual Integer -> Bool
prop_additive_inverse_integer = propInverse

{- Additive Double Properties -}

prop_additive_associativity_double :: Dual Double -> Dual Double -> Dual Double -> Bool
prop_additive_associativity_double x y z = propAssociative (approxRat x) (approxRat y) (approxRat z)

prop_additive_commutative_double :: Dual Double -> Dual Double -> Bool
prop_additive_commutative_double x y = propCommutative (approxRat x) (approxRat y)

prop_additive_identity_double :: Dual Double -> Bool
prop_additive_identity_double x = propIdentity (approxRat x)

prop_additive_inverse_double :: Dual Double -> Bool
prop_additive_inverse_double x = False --propInverse (approxRat x)

--return []
--runTests = $quickCheckAll

integerTestGroup :: TestTree
integerTestGroup =
  testGroup
    "Additive Integer Tests"
    [ testProperty "Additive Associativity" prop_additive_associativity_integer,
      testProperty "Additive Commutativity" prop_additive_commutative_integer,
      testProperty "Additive Identity" prop_additive_identity_integer,
      testProperty "Additive Inverse" prop_additive_inverse_integer
    ]

doubleTestGroup :: TestTree
doubleTestGroup =
  testGroup
    "Additive Double Tests"
    [ testProperty "Additive Associativity" prop_additive_associativity_double,
      testProperty "Additive Commutativity" prop_additive_commutative_double,
      testProperty "Additive Identity" prop_additive_identity_double,
      testProperty "Additive Inverse" prop_additive_inverse_double
    ]

tests :: TestTree
tests = testGroup "Additive Tests" [integerTestGroup, doubleTestGroup]