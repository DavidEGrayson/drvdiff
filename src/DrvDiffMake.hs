module DrvDiffMake where

import Derivation
import DrvDiff
import Data.List

makeDrvDiff :: Derivation -> Derivation -> DrvDiff
makeDrvDiff l r =
  DrvDiff
    (DrvPart
      outputsLeft
      inputsLeft
      sourcesLeft
      systemLeft
      builderLeft
      argsLeft
      envLeft)
    (DrvPart
      outputsRight
      inputsRight
      sourcesRight
      systemRight
      builderRight
      argsRight
      envRight)
  where
    (outputsLeft, outputsRight) = listDiff (drvOutputs l) (drvOutputs r)
    (inputsLeft, inputsRight) = listDiff (drvInputs l) (drvInputs r)
    (sourcesLeft, sourcesRight) = listDiff (drvSources l) (drvSources r)
    (systemLeft, systemRight) = itemDiff (drvSystem l) (drvSystem r)
    (builderLeft, builderRight) = itemDiff (drvBuilder l) (drvBuilder r)
    (argsLeft, argsRight) = itemDiff (drvArgs l) (drvArgs r)
    (envLeft, envRight) = listDiff (drvEnv l) (drvEnv r)

listDiff :: Eq t => [t] -> [t] -> ([t], [t])
listDiff l r = (l \\ r, r \\ l)

itemDiff :: Eq t => t -> t -> (Maybe t, Maybe t)
itemDiff l r | l == r = (Nothing, Nothing)
itemDiff l r = (Just l, Just r)
