module DrvFromAterm (drvFromAterm, BadDerivationAtermError) where

import Derivation
import Aterm

data BadDerivationAtermError =
  NoDeriveConstructor |
  NotAString |
  NotAList |
  NotAStringPair |
  NotAnOutput |
  NotAnInput |
  WrongConstructorArgCount
  deriving (Show)

drvFromAterm :: Aterm -> Either BadDerivationAtermError Derivation
drvFromAterm (Constructor "Derive" aterms) = drvFromDeriveArgs aterms
drvFromAterm _ = Left NoDeriveConstructor

drvFromDeriveArgs :: [Aterm] -> Either BadDerivationAtermError Derivation
drvFromDeriveArgs [outputs, inputs, sources, system, builder, args, env] =
  Derivation
  <$> listFromAterm outputFromAterm outputs
  <*> listFromAterm inputFromAterm inputs
  <*> listFromAterm stringFromAterm sources
  <*> stringFromAterm system
  <*> stringFromAterm builder
  <*> listFromAterm stringFromAterm args
  <*> listFromAterm stringPairFromAterm env
drvFromDeriveArgs _ = Left WrongConstructorArgCount

listFromAterm :: (Aterm -> Either BadDerivationAtermError a) -> Aterm
  -> Either BadDerivationAtermError [a]
listFromAterm itemDecoder (List aterms) = sequence (fmap itemDecoder aterms)
listFromAterm _ _ = Left NotAList

outputFromAterm :: Aterm -> Either BadDerivationAtermError DerivationOutput
outputFromAterm (Tuple [
    QuotedString name, QuotedString path,
    QuotedString unknown1, QuotedString unknown2
  ]) = Right (DerivationOutput (DerivationStr name) (DerivationStr path) (DerivationStr unknown1) (DerivationStr unknown2))
outputFromAterm _ = Left NotAnOutput

inputFromAterm :: Aterm -> Either BadDerivationAtermError DerivationInput
inputFromAterm (Tuple [QuotedString path, outputs]) =
  DerivationInput path <$> stringListFromAterm outputs
inputFromAterm _ = Left NotAnInput

stringListFromAterm :: Aterm -> Either BadDerivationAtermError [DerivationStr]
stringListFromAterm (List aterms) = sequence (fmap stringFromAterm aterms)
stringListFromAterm _ = Left NotAList

stringPairFromAterm :: Aterm -> Either BadDerivationAtermError DerivationEnvVar
stringPairFromAterm (Tuple [QuotedString string1, QuotedString string2]) =
  Right (DerivationEnvVar string1 string2)
stringPairFromAterm _ = Left NotAStringPair

stringFromAterm :: Aterm -> Either BadDerivationAtermError DerivationStr
stringFromAterm (QuotedString string) = pure (DerivationStr string)
stringFromAterm _ = Left NotAString
