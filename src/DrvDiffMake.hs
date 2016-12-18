module DrvDiffMake (makeDrvDiff) where

import Derivation
import DrvDiff
import Data.List

makeDrvDiff :: Derivation -> Derivation -> DrvDiff
makeDrvDiff drvLeft drvRight =
  DrvDiff
    (DrvPart
      outputsLeft
      []
      []
      Nothing
      Nothing
      Nothing
      [])
    (DrvPart
      outputsRight
      []
      []
      Nothing
      Nothing
      Nothing
      [])
  where
    (outputsLeft, outputsRight) = listDiff (drvOutputs drvLeft) (drvOutputs drvRight)

listDiff :: Eq t => [t] -> [t] -> ([t], [t])
listDiff l r = (l \\ r, r \\ l)
