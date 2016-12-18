module DrvDiffToString (drvDiffToString) where

import DrvDiff
import Derivation
import Data.List

drvDiffToString :: DrvDiff -> String
drvDiffToString (DrvDiff l r) =
  (drvPartToString "- " l) ++
  (drvPartToString "+ " r)

drvPartToString :: String -> DrvPart -> String
drvPartToString prefix part = let
    lines = (map drvOutputToString (drvPartOutputs part))
    lineFixer = \x -> prefix ++ x ++ "\n"
  in
    intercalate "" (map lineFixer lines)

drvOutputToString :: DerivationOutput -> String
drvOutputToString (DerivationOutput name path _ _) =
  "Output: " ++ name ++ ": " ++ path
