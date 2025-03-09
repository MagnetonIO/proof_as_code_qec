module Main where

import PenaltyTerms          (runPenaltyTerms)
import FunctorialMapping     (runFunctorialMapping)
import ProofNormalization    (runProofNormalization)
import Braiding              (runBraiding)
import ErrorCorrection       (runErrorCorrection)
import TypesAndInvariants    (runTypesAndInvariants)

main :: IO ()
main = do
  putStrLn "Starting QEC pipeline..."

  let initialState = "Initial QEC state"

  state1 <- runPenaltyTerms initialState
  state2 <- runFunctorialMapping state1
  state3 <- runProofNormalization state2
  state4 <- runBraiding state3
  state5 <- runErrorCorrection state4
  state6 <- runTypesAndInvariants state5

  putStrLn $ "Final QEC state: " ++ state6
  putStrLn "QEC Pipeline completed successfully."
