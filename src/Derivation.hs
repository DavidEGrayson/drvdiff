module Derivation where

data Derivation = Derivation
  {
    drvOutputs :: [DerivationOutput],
    drvInputs :: [DerivationInput],
    drvSources :: [DerivationStr],
    drvSystem :: DerivationStr,
    drvBuilder :: DerivationStr,
    drvArgs :: [DerivationStr],
    drvEnv :: [DerivationEnvVar]
  }
  deriving (Show, Eq)

data DerivationStr = DerivationStr String
  deriving (Show, Eq, Ord)

data DerivationOutput = DerivationOutput
  {
    drvOutputName :: DerivationStr,
    drvOutputPath :: DerivationStr,
    drvOutputUnknown1 :: DerivationStr,
    drvOutputUnknown2 :: DerivationStr
  }
  deriving (Show, Eq, Ord)

data DerivationInput = DerivationInput
  {
    drvInputPath :: String,
    drvInputNames :: [DerivationStr]
  }
  deriving (Show, Eq, Ord)

data DerivationEnvVar = DerivationEnvVar
  {
    drvEnvName :: String,
    drvEnvValue :: String
  }
  deriving (Show, Eq, Ord)
