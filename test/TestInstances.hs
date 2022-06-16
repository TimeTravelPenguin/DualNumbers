{-# LANGUAGE FlexibleInstances #-}

module TestInstances where

import Dual (Dual (Dual))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

{- QuickCheck Instances -}

instance Arbitrary (Dual Integer) where
  arbitrary = do
    x <- arbitrary
    Dual x <$> arbitrary

instance Arbitrary (Dual Double) where
  arbitrary = do
    x <- arbitrary
    Dual x <$> arbitrary
