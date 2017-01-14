module Derivation where

import Data.Maybe (listToMaybe)
import Data.List.Split

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

drvEnvVar :: String -> Derivation -> Maybe String
drvEnvVar name drv = fmap drvEnvValue
  (listToMaybe (filter (\ev -> (drvEnvName ev) == name) (drvEnv drv)))

drvEnvVarAsSet :: String -> Derivation -> [String]
drvEnvVarAsSet name drv =
  case drvEnvVar name drv of
    Nothing -> []
    Just str -> (split . dropInitBlank . dropFinalBlank . condense . dropDelims . oneOf) " \t\n" str
    -- TODO: Why is this sometimes returning empty strings?  And why are there
    -- empty strings at the end and the beginning of the list?
