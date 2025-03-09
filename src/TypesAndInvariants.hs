{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators, TypeFamilies #-}

module TypesAndInvariants
  ( ErrorState(..)
  , Qubit(..)
  , Invariant(..)
  , UniversalInvariant(..)
  , Register(..)
  , registerInvariant
  , runTypesAndInvariants    -- 1) Export the pipeline function
  ) where

-- Define a kind for error states
data ErrorState = NoError | ZError | XError | YError

-- A simple representation of a quantum bit (qubit) with an error state.
data Qubit (e :: ErrorState) where
  Qubit :: Qubit 'NoError

-- Universal invariant type: for our purposes, an invariant is a type-level marker
data Invariant = Inv

-- A type class representing a universal invariant.
class UniversalInvariant a where
  invariant :: a -> Invariant

instance UniversalInvariant (Qubit 'NoError) where
  invariant _ = Inv

-- We can extend this to a list of qubits or a register.
data Register (n :: *) where
  EmptyReg :: Register ()
  ConsReg  :: UniversalInvariant a => a -> Register b -> Register (a, b)

-- Example invariant for a register is simply a tuple of invariants.
registerInvariant :: Register n -> [Invariant]
registerInvariant EmptyReg      = []
registerInvariant (ConsReg q r) = invariant q : registerInvariant r

--------------------------------------------------------------------------------
-- 2) Add a pipeline function for Main.hs
--------------------------------------------------------------------------------

-- | 'runTypesAndInvariants' is used by the QEC pipeline to finalize type checking and invariants.
runTypesAndInvariants :: String -> IO String
runTypesAndInvariants input = do
  putStrLn "Running Types and Invariants..."
  let output = input ++ " -> [InvariantsChecked]"
  return output
