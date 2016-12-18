module DrvDiffPrint (printDrvDiff) where

import DrvDiff

printDrvDiff :: DrvDiff -> IO ()
printDrvDiff (DrvDiff l r) = do
  printDrvPart "- " l
  printDrvPart "+ " r

printDrvPart :: String -> DrvPart -> IO ()
printDrvPart prefix part = do
  putStrLn $ prefix ++ (show (drvPartOutputs part))
