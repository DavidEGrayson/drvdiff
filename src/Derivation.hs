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
  NotAString |
  NotAList |
  WrongConstructorArgCount
  deriving (Show)

drvFromAterm :: Aterm -> Either BadDerivationAtermError Derivation
drvFromAterm (Constructor "Derive" aterms) = drvFromDeriveArgs aterms
drvFromAterm _ = Left NoDeriveConstructor

stringFromAterm :: Aterm -> Either BadDerivationAtermError String
stringFromAterm (QuotedString string) = pure string
stringFromAterm _ = Left NotAString

stringListFromAterm :: Aterm -> Either BadDerivationAtermError [String]
stringListFromAterm (List aterms) = stringListFromAtermList aterms
stringListFromAterm _ = Left NotAList

stringListFromAtermList :: [Aterm] -> Either BadDerivationAtermError [String]
stringListFromAtermList list = sequence (fmap stringFromAterm list)

drvFromDeriveArgs :: [Aterm] -> Either BadDerivationAtermError Derivation
drvFromDeriveArgs [outputs, inputs, sources, system, builder, args, env] =
  Derivation
  <$> pure []  -- TODO: fix
  <*> pure []  -- TODO: fix
  <*> stringListFromAterm sources
  <*> stringFromAterm system
  <*> stringFromAterm builder
  <*> stringListFromAterm args
  <*> pure []  -- TODO: fix
drvFromDerivArgs _ = Left WrongConstructorArgCount
