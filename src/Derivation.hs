module Derivation (Derivation, derivationFromAterm, BadDerivationAtermError) where

import Aterm

data Derivation = Foo String
  deriving (Show)

data BadDerivationAtermError = FooSomething
  deriving (Show)

derivationFromAterm :: Aterm -> Either BadDerivationAtermError Derivation
derivationFromAterm a = Right (Foo "tmphax")
