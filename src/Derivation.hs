module Derivation where

data Derivation = Derivation
  {
    drvOutputs :: [DerivationOutput],
    drvInputs :: [DerivationInput],
    drvSources :: [String],
    drvSystem :: String,
    drvBuilder :: String,
    drvArgs :: [String],
    drvEnv :: [DerivationEnvVar]
  }
  deriving (Show, Eq)

data DerivationOutput = DerivationOutput
  {
    drvOutputName :: String,
    drvOutputPath :: String,
    drvOutputUnknown1 :: String,
    drvOutputUnknown2 :: String
  }
  deriving (Show, Eq, Ord)

data DerivationInput = DerivationInput
  {
    drvInputPath :: String,
    drvInputNames :: [String]
  }
  deriving (Show, Eq, Ord)

data DerivationEnvVar = DerivationEnvVar
  {
    drvEnvName :: String,
    drvEnvValue :: String
  }
  deriving (Show, Eq, Ord)
