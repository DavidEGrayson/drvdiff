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
  NotAStringPair |
  WrongConstructorArgCount
  deriving (Show)

drvFromAterm :: Aterm -> Either BadDerivationAtermError Derivation
drvFromAterm (Constructor "Derive" aterms) = drvFromDeriveArgs aterms
drvFromAterm _ = Left NoDeriveConstructor

drvFromDeriveArgs :: [Aterm] -> Either BadDerivationAtermError Derivation
drvFromDeriveArgs [outputs, inputs, sources, system, builder, args, env] =
  Derivation
  <$> pure []  -- TODO: fix
  <*> pure []  -- TODO: fix
  <*> stringListFromAterm sources
  <*> stringFromAterm system
  <*> stringFromAterm builder
  <*> stringListFromAterm args
  <*> stringPairsFromAterm env  -- TODO: fix
drvFromDerivArgs _ = Left WrongConstructorArgCount

stringListFromAterm :: Aterm -> Either BadDerivationAtermError [String]
stringListFromAterm (List aterms) = sequence (fmap stringFromAterm aterms)
stringListFromAterm _ = Left NotAList

stringPairsFromAterm :: Aterm -> Either BadDerivationAtermError [(String, String)]
stringPairsFromAterm (List aterms) = sequence (fmap stringPairFromAterm aterms)
stringPairsFromAterm _ = Left NotAList

stringPairFromAterm :: Aterm -> Either BadDerivationAtermError (String, String)
stringPairFromAterm (Tuple [(QuotedString string1), (QuotedString string2)]) =
  Right (string1, string2)
stringPairFromAterm _ = Left NotAStringPair

stringFromAterm :: Aterm -> Either BadDerivationAtermError String
stringFromAterm (QuotedString string) = pure string
stringFromAterm _ = Left NotAString
