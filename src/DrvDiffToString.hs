module DrvDiffToString (drvDiffToString) where

import DrvDiff

drvDiffToString :: DrvDiff -> String
drvDiffToString (DrvDiff l r) =
  (drvPartToString "- " l) ++ "\n" ++
  (drvPartToString "+ " r) ++ "\n"

drvPartToString :: String -> DrvPart -> String
drvPartToString prefix part =
  prefix ++ (show (drvPartOutputs part))
