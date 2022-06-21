{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Dual
  ( Dual (..),
  )
where

import qualified Algebra.Absolute as Absolute
import qualified Algebra.Additive as Additive
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Field as Field
import qualified Algebra.Module as Module
import qualified Algebra.ModuleBasis as ModuleBasis
import Algebra.Monoid (idt)
import qualified Algebra.Monoid as Monoid
import qualified Algebra.OccasionallyScalar as OccasionallyScalar
import qualified Algebra.Ring as Ring
import qualified Algebra.ToRational as ToRational
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.Vector as Vector
import qualified Algebra.VectorSpace as VectorSpace
import Algebra.ZeroTestable (defltIsZero)
import qualified Algebra.ZeroTestable as ZeroTestable
import ErrorTypes (DualError (RootError, ToScalarError), raiseError)
import NumericPrelude

-- |
--    The hyper-complex number system with imaginary component epsilon, such that epsilon squared equals zero.
--    See: https://en.wikipedia.org/wiki/Dual_number
type Real a = a

type Imaginary a = a

data Dual a = Dual !(Real a) !(Imaginary a) deriving (Eq)

{- General Instance -}

instance (Show a, Ord a, Ring.C a) => Show (Dual a) where
  show (Dual x y) =
    let formatSgn n =
          if n >= zero
            then "+" ++ show n
            else show n
     in concat [show x, formatSgn y, "ε"]

{- NumericPrelude Instances -}

instance Ring.C a => Monoid.C (Dual a) where
  idt = Dual one zero
  (<*>) = (+)

instance (Ring.C a, Module.C a a) => Module.C a (Dual a) where
  (*>) a (Dual x y) = Dual (a *> x) (a *> y)

instance Module.C a a => ModuleBasis.C a (Dual a) where
  basis _ = [Dual 1 0, Dual 0 1]
  flatten (Dual a b) = [a, b]
  dimension _ _ = 2

instance (Field.C a, Module.C a a) => VectorSpace.C a (Dual a)

instance (Additive.C a, Ring.C a) => Additive.C (Dual a) where
  zero = Dual zero zero
  (+) (Dual x y) (Dual p q) = Dual (x + p) (y + q)
  negate (Dual x y) = Dual (negate x) (negate y)

instance (Algebraic.C a, ToRational.C a) => Algebraic.C (Dual a) where
  root n (Dual x y) =
    let n' = 1 / fromInteger n
     in if isZero x
          then raiseError "Dual.root" RootError
          else Dual (Algebraic.power n' x) (n' * y * Algebraic.power (n' - 1) x)

instance (Ring.C a, Field.C a) => Field.C (Dual a) where
  (/) (Dual x y) (Dual p q) = Dual (x / p) ((y * p - x * q) / p ^ 2)

instance (Ring.C a) => Ring.C (Dual a) where
  (*) (Dual x y) (Dual p q) = Dual (x * p) (x * q + p * y)
  one = idt

instance (Ring.C a, Eq a) => ZeroTestable.C (Dual a) where
  isZero = defltIsZero

instance (Ring.C a, Eq a, ZeroTestable.C a) => OccasionallyScalar.C a (Dual a) where
  toScalar = dualToScalar
  toMaybeScalar = dualToMaybeScalar
  fromScalar a = Dual a 0

instance (Algebraic.C a, ToRational.C a, Transcendental.C a) => Transcendental.C (Dual a) where
  -- TODO:
  --  1. Compare sinh, cosh, tanh with exponential forms (Analysis of Dual Functions, section 3.3)
  pi = Dual pi 0
  exp (Dual x y) = Dual (exp x) (y * exp x)
  log (Dual x y) = Dual (log x) (y / x) -- Do we need to define logBase?
  (**) (Dual x y) (Dual u v) = undefined -- TODO: How do we define this?
  sin (Dual x y) = Dual (sin x) (y * cos x)
  cos (Dual x y) = Dual (cos x) (-y * sin x)
  tan z = sin z / cos z -- Compare: tan (Dual x y) = Dual (tan x) (-y / (cos x)^2)
  asin (Dual x y) = undefined
  acos (Dual x y) = undefined
  atan (Dual x y) = undefined
  sinh (Dual x y) = Dual (sinh x) (y * cosh x)
  cosh (Dual x y) = Dual (cosh x) (y * sinh x)
  tanh z = sinh z / cosh z
  asinh (Dual x y) = undefined
  acosh (Dual x y) = undefined
  atanh (Dual x y) = undefined

{- Instance Functions -}

dualToScalar :: (Eq a, Ring.C a, ZeroTestable.C a) => Dual a -> a
dualToScalar (Dual x y) =
  if isZero y
    then x
    else raiseError "Dual.dualToScalar" ToScalarError

dualToMaybeScalar :: ZeroTestable.C a => Dual a -> Maybe a
dualToMaybeScalar (Dual x y) =
  if isZero y
    then Just x
    else Nothing