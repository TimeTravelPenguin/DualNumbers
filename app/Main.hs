{-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Algebra.Differential as Differential
import qualified Algebra.Ring as Ring
import Algebra.Algebraic
import Dual
import NumericPrelude
import Prelude (IO, print, putStrLn, ($))
import MathObj.PowerSeries as PS

mkDual :: Integer -> Integer -> Dual Integer
mkDual = Dual

mkDualF :: Double -> Double -> Dual Double
mkDualF = Dual

fx :: Ring.C a => a -> a
fx x = x ^ 2

main :: IO ()
main = do
  let x = mkDualF 1 2
  let y = mkDualF (-1) 3
  let z = rootD 2 x
  print z
  print $ fx z
  putStrLn "Done"
