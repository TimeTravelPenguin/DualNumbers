{-# LANGUAGE RebindableSyntax #-}

module Main where

import Algebra.Algebraic
import qualified Algebra.Differential as Differential
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

getDualComp :: Ring.C a => Dual a -> a
getDualComp (Dual _ b) = b

derivateAt f x0 =
  let d = Dual x0 1
   in getDualComp $ f d

fx x = sin x ^ 2

f'x x = 2 * sin x * cos x

main :: IO ()
main = do
  let z = mkDualF 1 1
  print z
  print $ derivateAt fx (1 :: Double)
  print $ getDualComp $ f'x z
  putStrLn "Done"
