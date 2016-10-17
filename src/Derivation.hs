module Derivation (Derivation, drvFromAterm, BadDerivationAtermError) where

import Aterm

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
  NoDeriveConstructor |
  NotAString
  deriving (Show)

drvFromAterm :: Aterm -> Either BadDerivationAtermError Derivation
drvFromAterm (Constructor "Derive" aterms) = drvFromDeriveArgs aterms
drvFromAterm _ = Left NoDeriveConstructor

stringFromAterm :: Aterm -> Either BadDerivationAtermError String
stringFromAterm (QuotedString string) = pure string
stringFromAterm _ = Left NotAString

drvFromDeriveArgs :: [Aterm] -> Either BadDerivationAtermError Derivation
drvFromDeriveArgs [outputs, inputs, sources, system, builder, args, env] =
  Derivation <$> (pure []) <*> (pure []) <*> (pure []) <*> stringFromAterm system <*> (pure "") <*> (pure []) <*> (pure [])
  -- TODO: don't use dummy values above, use real values
