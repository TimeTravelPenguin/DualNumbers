module Helpers where

import Dual (Dual (Dual))
import qualified Number.Ratio
import NumericPrelude
import Prelude ()

{- Helper Constants -}

eps :: Double
eps = 10e-12

{- Helper functions -}

approxRat :: Dual Double -> Dual (Number.Ratio.T Integer)
approxRat (Dual x y) = Dual (approxRational x eps) (approxRational y eps)