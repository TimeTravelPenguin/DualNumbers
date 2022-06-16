{-# LANGUAGE RebindableSyntax #-}

module Dual
  ( Dual (..),
  )
where

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

data Dual a = Dual a a

instance (Show a, Ord a, Ring.C a) => Show (Dual a) where
  show (Dual x y) =
    let formatSgn n =
          if n >= zero
            then "+" ++ show n
            else show n
     in concat [show x, formatSgn y, "ε"]

absD (Dual x y) = Dual (abs x) zero

instance Ring.C a => Monoid.C (Dual a) where
  idt = Dual 1 0
  (<*>) = (+)

instance (Additive.C a, Ring.C a) => Additive.C (Dual a) where
  zero = Dual zero zero
  (+) (Dual x y) (Dual p q) = Dual (x + p) (y + q)
  negate (Dual x y) = Dual (negate x) (negate y)

instance (Algebraic.C a, ToRational.C a, Eq a) => Algebraic.C (Dual a) where
  root n (Dual x y) =
    let n' = 1 / fromInteger n
     in if x == zero
          then error "Dual.rootD : a = 0"
          else Dual (Algebraic.power n' x) (n' * y * Algebraic.power (n' - 1) x)

instance (Ring.C a, Field.C a) => Field.C (Dual a) where
  (/) (Dual x y) (Dual p q) = Dual (x / p) ((y * p - x * q) / p ^ 2)

instance (Ring.C a) => Ring.C (Dual a) where
  (*) (Dual x y) (Dual p q) = Dual (x * p) (x * q + p * y)
  one = idt

instance (Algebraic.C a, ToRational.C a, Transcendental.C a) => Transcendental.C (Dual a) where
  pi = Dual pi 0
  exp = undefined
  log = undefined
  sin (Dual a b) = Dual (sin a) (b * cos a)
  cos (Dual a b) = Dual (cos a) (-b * sin a)
  atan = undefined
