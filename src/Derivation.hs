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

--instance Type DerivationFamily [(String, String)] where
--    constructors = [Concr DerivationF]
