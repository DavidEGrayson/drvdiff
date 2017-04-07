module DrvDiffMake where

import Derivation
import DrvDiff
import Data.List

makeDrvDiff :: Derivation -> Derivation -> DrvDiff
makeDrvDiff l r = uncurry DrvDiff $
  (DrvPart, DrvPart)
    `tuply` diffWith listDiff drvOutputs
    `tuply` diffWith listDiff drvInputs
    `tuply` diffWith listDiff drvSources
    `tuply` diffWith itemDiff drvSystem
    `tuply` diffWith itemDiff drvBuilder
    `tuply` diffWith itemDiff drvArgs
    `tuply` diffWith listDiff drvEnv
  where
    diffWith :: (p -> p -> d) -> (Derivation -> p) -> d
    diffWith d f = d (f l) (f r)

tuply :: (t3 -> t2, t1 -> t) -> (t3, t1) -> (t2, t)
tuply (part1, part2) (prop1, prop2) = (part1 prop1, part2 prop2)

listDiff :: Eq t => [t] -> [t] -> ([t], [t])
listDiff l r = (l \\ r, r \\ l)

itemDiff :: Eq t => t -> t -> (Maybe t, Maybe t)
itemDiff l r | l == r = (Nothing, Nothing)
itemDiff l r = (Just l, Just r)
