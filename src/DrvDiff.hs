module DrvDiff where

import Derivation

data DrvDiff = DrvDiff DrvPart DrvPart
  deriving (Show)

data DrvPart = DrvPart
  {
    drvPartOutputs :: [DerivationOutput],
    drvPartInputs :: [DerivationInput],
    drvPartSources :: [String],
    drvPartSystem :: Maybe String,
    drvPartBuilder :: Maybe String,
    drvPartArgs :: Maybe [String],
    drvPartEnv :: [DerivationEnvVar]
  }
  deriving (Show)
