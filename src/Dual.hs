{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}

module Dual where

import qualified Algebra.Absolute as Absolute
import qualified Algebra.Additive as Additive
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import Algebra.Monoid (idt)
import qualified Algebra.Monoid as Monoid
import qualified Algebra.Ring as Ring
import qualified Algebra.ToRational as ToRational
import qualified Algebra.Transcendental as Transcendental
import NumericPrelude

-- See: https://stackoverflow.com/questions/42067603/different-type-constraints-for-the-same-instance

data Dual a where
  Dual :: a -> a -> Dual a
  Imaginary :: Dual a

instance (Show a, Ord a, Ring.C a) => Show (Dual a) where
  show (Dual x y) =
    let formatSgn x =
          if x >= zero
            then "+" ++ show x
            else show x
     in concat [show x, formatSgn y, "ε"]
  show Imaginary = "ε"

addD :: (Additive.C a, Ring.C a) => Dual a -> Dual a -> Dual a
addD (Dual x y) (Dual p q) = Dual (x + p) (y + q)
addD (Dual x y) Imaginary = Dual x (y + 1)
addD Imaginary (Dual p q) = Dual p (q + 1)
addD Imaginary Imaginary = Dual zero 2

mulD :: (Additive.C a, Ring.C a) => Dual a -> Dual a -> Dual a
mulD (Dual x y) (Dual p q) = Dual (x * p) (x * q + p * y)
mulD (Dual x y) Imaginary = Dual zero x
mulD Imaginary (Dual p q) = Dual zero q
mulD Imaginary Imaginary = Dual zero zero

absD (Dual x y) = Dual (abs x) zero
absD Imaginary = Dual zero zero

negateD :: (Additive.C a, Ring.C a) => Dual a -> Dual a
negateD (Dual x y) = Dual (negate x) (negate y)
negateD Imaginary = Dual zero (-1)

rootD :: (Algebraic.C a, ToRational.C a) => Integer -> Dual a -> Dual a
rootD n (Dual x y) =
  let n' = 1 / fromInteger n
   in if x == zero
        then error "Dual.rootD : a = 0"
        else Dual (Algebraic.power n' x) (n' * y * Algebraic.power (n' - 1) x)
rootD n Imaginary = error "Dual.rootD : a = 0"

instance Ring.C a => Monoid.C (Dual a) where
  idt = Dual 1 0
  (<*>) = addD

instance (Algebraic.C a, ToRational.C a, Eq a) => Algebraic.C (Dual a) where
  root = rootD

instance (Ring.C a, Field.C a) => Field.C (Dual a) where
  (/) (Dual a b) (Dual c d) = Dual (a / c) ((b * c - a * d) / c ^ 2)
  (/) x Imaginary = x / Dual 0 1
  (/) Imaginary y = Dual 0 1 / y

instance (Ring.C a) => Ring.C (Dual a) where
  (*) = mulD
  one = idt

instance (Additive.C a, Ring.C a) => Additive.C (Dual a) where
  zero = Dual zero zero
  (+) = addD
  negate = negateD