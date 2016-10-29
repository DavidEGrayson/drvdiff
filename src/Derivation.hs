module Derivation where

import Data.Generic.Diff

data Derivation = Derivation
  {
    drvOutputs :: [DerivationOutput],
    drvInputs :: [DerivationInput],
    drvSources :: [String],
    drvSystem :: String,
    drvBuilder :: String,
    drvArgs :: [String],
    drvEnv :: [(String, String)]
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

data DerivationFamily :: * -> * -> * where
    DerivationF :: DerivationFamily Derivation
      (Cons [DerivationOutput]
      (Cons [DerivationInput]
      (Cons [String]
      (Cons String
      (Cons String
      (Cons [String]
      (Cons [(String, String)]
      Nil)))))))
    -- DerivationOutputF :: DerivationFamily DerivationOutput
    --   (Cons String
    --   (Cons String
    --   (Cons String
    --   (Cons String
    --   Nil))))
    -- DerivationInputF :: DerivationFamily DerivationInput
    --   (Cons String
    --   (Cons [String]
    --   Nil))
