{-# LANGUAGE DataKinds, GADTs, TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}

module FunctorialMapping
  ( ErrorFunctor(..)
  , mapError
  , LocalError(..)
  , runFunctorialMapping      -- 1) Export the pipeline function
  ) where

import TypesAndInvariants
import ErrorCorrection

-- Define a functor class that maps from a local error state to a corrected invariant.
class ErrorFunctor f where
  fmapError :: (Correctable e) => f e -> Qubit 'NoError

-- Our local error functor wraps a qubit.
newtype LocalError e = LocalError { getQubit :: Qubit e }

instance ErrorFunctor LocalError where
  fmapError (LocalError q) = correctQubit q

-- A helper function to perform the mapping.
mapError :: (ErrorFunctor f, Correctable e) => f e -> Qubit 'NoError
mapError = fmapError

--------------------------------------------------------------------------------
-- 2) Add a pipeline function for Main.hs
--------------------------------------------------------------------------------

-- | 'runFunctorialMapping' is a simple function used by the QEC pipeline.
--   Adjust to integrate real logic if desired.
runFunctorialMapping :: String -> IO String
runFunctorialMapping input = do
  putStrLn "Running Functorial Mapping..."
  let output = input ++ " -> [FunctorialMappingApplied]"
  return output
