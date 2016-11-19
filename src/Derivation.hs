module Derivation where

import Data.Generic.Diff

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

data DerivationFamily :: * -> * -> * where
    DerivationF :: DerivationFamily Derivation
      (Cons [DerivationOutput]
      (Cons [DerivationInput]
      (Cons [DerivationStr]
      (Cons DerivationStr
      (Cons DerivationStr
      (Cons [DerivationStr]
      (Cons [DerivationEnvVar]
      Nil)))))))
    DerivationOutputF :: DerivationFamily DerivationOutput
      (Cons DerivationStr
      (Cons DerivationStr
      (Cons DerivationStr
      (Cons DerivationStr
      Nil))))
    DerivationInputF :: DerivationFamily DerivationInput
      (Cons String
      (Cons [String]
      Nil))
    DerivationStrF :: DerivationFamily DerivationStr
      (Cons String Nil)

instance Family DerivationFamily where
    decEq DerivationF DerivationF = Just (Refl, Refl)
    fields DerivationF (Derivation drvOutputs drvInputs drvSources drvSystem drvBuilder drvArgs drvEnv)
       = Just (CCons drvOutputs
              (CCons drvInputs
              (CCons drvSources
              (CCons drvSystem
              (CCons drvBuilder
              (CCons drvArgs
              (CCons drvEnv
              CNil)))))))
    apply DerivationF (CCons drvOutputs
              (CCons drvInputs
              (CCons drvSources
              (CCons drvSystem
              (CCons drvBuilder
              (CCons drvArgs
              (CCons drvEnv
              CNil))))))) = (Derivation drvOutputs drvInputs drvSources drvSystem drvBuilder drvArgs drvEnv)
    string DerivationF = "DerivationF"

instance Type DerivationFamily Derivation where
    constructors = [Concr DerivationF]

instance Type DerivationFamily [DerivationStr] where
  constructors = []

instance Type DerivationFamily [DerivationOutput] where
  constructors = []

instance Type DerivationFamily [DerivationInput] where
  constructors = []

instance Type DerivationFamily [DerivationEnvVar] where
  constructors = []

instance Type DerivationFamily DerivationStr where
  constructors = []

