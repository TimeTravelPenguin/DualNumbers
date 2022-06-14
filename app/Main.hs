{-# LANGUAGE RebindableSyntax #-}

module Main where

import Algebra.Algebraic
import Dual
import NumericPrelude
import Prelude (IO, print, putStrLn, ($))

mkDual :: Integer -> Integer -> Dual Integer
mkDual = Dual

mkDualF :: Double -> Double -> Dual Double
mkDualF = Dual

main :: IO ()
main = do
  let x = mkDualF 1 2
  let y = mkDualF (-1) 3
  let z = rootD 2 x
  print z
  putStrLn "Done"
