{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}

module ProofNormalization
  ( normalizeProof
  , runProofNormalization     -- 1) Export the pipeline function
  ) where

import TypesAndInvariants
import ErrorCorrection

-- A simple normalization: given a qubit with an error, normalize it to the error-free state.
normalizeProof :: Correctable e => Qubit e -> Qubit 'NoError
normalizeProof = correctQubit

-- In a more advanced system, normalization would use type-level rewriting rules.

--------------------------------------------------------------------------------
-- 2) Add a pipeline function for Main.hs
--------------------------------------------------------------------------------

-- | 'runProofNormalization' is used by the QEC pipeline to illustrate proof normalization.
runProofNormalization :: String -> IO String
runProofNormalization input = do
  putStrLn "Running Proof Normalization..."
  let output = input ++ " -> [ProofNormalizationApplied]"
  return output
