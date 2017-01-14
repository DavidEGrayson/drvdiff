module DrvDiffToString where

import DrvDiff
import Derivation
import Data.List
import Data.Maybe

drvDiffToString :: DrvDiff -> String
drvDiffToString (DrvDiff l r) =
  (drvPartToString "- " l) ++
  (drvPartToString "+ " r)

drvPartToString :: String -> DrvPart -> String
drvPartToString prefix p =
  joinLines prefix (
    (fmap drvOutputToString (drvPartOutputs p)) ++
    (fmap drvInputToString (drvPartInputs p)) ++
    (fmap drvSourceToString (drvPartSources p)) ++
    maybeToList (fmap drvSystemToString (drvPartSystem p)) ++
    maybeToList (fmap drvBuilderToString (drvPartBuilder p)) ++
    maybeToList (fmap drvArgsToString (drvPartArgs p)) ++
    (fmap drvEnvVarToString (drvPartEnv p))
  )

joinLines :: String -> [String] -> String
joinLines prefix ls = intercalate "" (map (\x -> prefix ++ x ++ "\n") ls)

setDiffToString :: ([String], [String]) -> String
setDiffToString (l, r) = (joinLines "- " l) ++ (joinLines "+ " r)

drvOutputToString :: DerivationOutput -> String
drvOutputToString (DerivationOutput name path _ _) =
  "Output: " ++ name ++ ": " ++ path

drvInputToString :: DerivationInput -> String
drvInputToString (DerivationInput path names) =
  "Input: " ++ path ++ " " ++ (intercalate "," names)

drvSourceToString :: String -> String
drvSourceToString s = "source: " ++ s

drvSystemToString :: String -> String
drvSystemToString s = "system: " ++ s

drvBuilderToString :: String -> String
drvBuilderToString b = "builder: " ++ b

drvArgsToString :: [String] -> String
drvArgsToString args = "args: " ++ intercalate " " args

drvEnvVarToString :: DerivationEnvVar -> String
drvEnvVarToString (DerivationEnvVar name value) = "env: " ++ name ++ " = " ++ (show value)
