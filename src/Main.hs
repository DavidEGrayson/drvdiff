module Main where

import Derivation
import DrvFromAterm
import DrvDiffMake
import DrvDiffToString
import Parser
import System.Environment
import System.Exit
import System.IO

data MainAction =
     CompareWholeDrv |
     CompareEnvVarAsSet String

main :: IO ()
main = do
  (drvFilenameA, drvFilenameB, mainAction) <- parseArgs
  drvA <- filenameToDrv drvFilenameA
  drvB <- filenameToDrv drvFilenameB
  case mainAction of
    CompareWholeDrv -> do
      diff <- pure $ makeDrvDiff drvA drvB
      putStr $ drvDiffToString diff
    CompareEnvVarAsSet envVarName -> do
      diff <- pure $ listDiff (drvEnvVarAsSet envVarName drvA)
                              (drvEnvVarAsSet envVarName drvB)
      putStr $ setDiffToString diff

parseArgs :: IO (String, String, MainAction)
parseArgs = do
  args <- getArgs
  case args of
    [filenameA, filenameB] -> pure $ (filenameA, filenameB, CompareWholeDrv)
    [filenameA, filenameB, "-e", envVarName] -> pure $ (filenameA, filenameB, CompareEnvVarAsSet envVarName)
    _ -> printErrorStringAndExit "Usage: drvdiff FILENAME FILENAME [OPTIONS]"

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
