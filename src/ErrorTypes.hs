module ErrorTypes (DualError (..), raiseError) where

type Caller = String

data DualError
  = RootError
  | ToScalarError

raiseError :: Caller -> DualError -> a
raiseError caller RootError = error $ unwords [caller, ": cannot take the root of a dual with real part equal to zero."]
raiseError caller ToScalarError = error $ unwords [caller, ": cannot convert a dual to a scalar with non-real part equal to a non-zero value."]