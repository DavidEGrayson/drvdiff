module Main where

import Derivation
import DrvFromAterm
import DrvDiffMake
import DrvDiffPrint
import Parser
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  (drvFilenameA, drvFilenameB) <- parseArgs
  drvA <- filenameToDrv drvFilenameA
  drvB <- filenameToDrv drvFilenameB
  diff <- pure $ makeDrvDiff drvA drvB
  printDrvDiff diff

parseArgs :: IO (String, String)
parseArgs = do
  args <- getArgs
  case args of
    [filenameA, filenameB] -> pure $ (filenameA, filenameB)
    _ -> printErrorStringAndExit "Usage: drvdiff FILENAME FILENAME"

filenameToDrv :: String -> IO Derivation
filenameToDrv drvFilename = do
  drvString <- readFile drvFilename
  drvAterm <- case parseAterm drvFilename drvString of
    Left err -> printErrorAndExit err
    Right aterm -> pure $ aterm
  case drvFromAterm drvAterm of
    Left err -> printErrorAndExit err
    Right drv -> pure $ drv

printErrorAndExit :: Show e => e -> IO a
printErrorAndExit err = printErrorStringAndExit (show err)

printErrorStringAndExit :: String -> IO a
printErrorStringAndExit err = do
  hPutStrLn stderr $ err
  exitWith (ExitFailure 1)
