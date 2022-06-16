{-# LANGUAGE RebindableSyntax #-}

module Main where

import Algebra.Algebraic
import qualified Algebra.Differential as Differential
import Algebra.OccasionallyScalar (C (fromScalar))
import qualified Algebra.OccasionallyScalar as OccasionallyScalar
import qualified Algebra.Ring as Ring
import qualified Algebra.Transcendental as Transcendental
import Dual
import MathObj.PowerSeries as PS
import NumericPrelude
import Prelude (IO, print, putStrLn, ($))

mkDual :: Integer -> Integer -> Dual Integer
mkDual = Dual

mkDualF :: Double -> Double -> Dual Double
mkDualF = Dual

getImaginary :: Ring.C a => Dual a -> a
getImaginary (Dual _ b) = b

derivateAt :: Ring.C a => (Dual a -> t) -> a -> t
derivateAt f x = f (Dual x 1)

fx x = sin x ^ 2

f'x x = 2 * sin x * cos x

main :: IO ()
main = do
  let z = mkDualF 1 1
  print z
  let dx = derivateAt fx 1 :: Dual Double
  print dx
  print $ f'x (1 :: Double)
  putStrLn "Done"
