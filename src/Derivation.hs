module Derivation (Derivation, drvFromAterm, BadDerivationAtermError) where

import Aterm

data Derivation = Derivation
  {
    drvOutputs :: [DerivationOutput],
    drvInputs :: [DerivationInput],
    drvBuilders :: [String]
    -- TODO: add rest of fields
  }
  deriving (Show)

data DerivationOutput = DerivationOutput
  {
    drvOutputName :: String,
    drvOutputPath :: String,
    drvOutputUnknown1 :: String,
    drvOutputUnknown2 :: String
  }
  deriving (Show)

data DerivationInput = DerivationInput
  {
    drvInputPath :: String,
    drvInputNames :: [String]
  }
  deriving (Show)

data BadDerivationAtermError =
  WrongOverallStructure
  deriving (Show)

drvFromAterm :: Aterm -> Either BadDerivationAtermError Derivation
drvFromAterm (Constructor "Derive" [outputs, inputs, builders, _, _, _, _]) =
  Right (Derivation [] [] [])
drvFromAterm _ = Left WrongOverallStructure
