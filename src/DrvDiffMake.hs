module DrvDiffMake where

import Derivation
import DrvDiff
import Data.List

makeDrvDiff :: Derivation -> Derivation -> DrvDiff
makeDrvDiff l r =
  DrvDiff
    (DrvPart
      outputsLeft inputsLeft sourcesLeft
      systemLeft builderLeft argsLeft envLeft)
    (DrvPart
      outputsRight inputsRight sourcesRight
      systemRight builderRight argsRight envRight)
  where
    (outputsLeft, outputsRight) = diffWith listDiff drvOutputs
    (inputsLeft, inputsRight) = diffWith listDiff drvInputs
    (sourcesLeft, sourcesRight) = diffWith listDiff drvSources
    (systemLeft, systemRight) = diffWith itemDiff drvSystem
    (builderLeft, builderRight) = diffWith itemDiff drvBuilder
    (argsLeft, argsRight) = diffWith itemDiff drvArgs
    (envLeft, envRight) = diffWith listDiff drvEnv
    diffWith :: (p -> p -> d) -> (Derivation -> p) -> d
    diffWith d f = d (f l) (f r)

listDiff :: Eq t => [t] -> [t] -> ([t], [t])
listDiff l r = (l \\ r, r \\ l)

itemDiff :: Eq t => t -> t -> (Maybe t, Maybe t)
itemDiff l r | l == r = (Nothing, Nothing)
itemDiff l r = (Just l, Just r)
