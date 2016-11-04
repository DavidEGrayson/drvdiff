module Main where

import Derivation
import DrvFromAterm
import Parser
import System.Environment
import System.Exit
import System.IO

import Data.Generic.Diff

main :: IO ()
main = do
  (drvFilenameA, drvFilenameB) <- parseArgs
  drvA <- filenameToDrv drvFilenameA
  drvB <- filenameToDrv drvFilenameB
  putStrLn $ show drvA
  putStrLn $ show (diff drvA drvB)

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
    Left error -> printErrorAndExit error
    Right aterm -> pure $ aterm
  case drvFromAterm drvAterm of
    Left error -> printErrorAndExit error
    Right drv -> pure $ drv

printErrorAndExit :: Show e => e -> IO a
printErrorAndExit error = printErrorStringAndExit (show error)

printErrorStringAndExit :: String -> IO a
printErrorStringAndExit error = do
  hPutStrLn stderr $ error
  exitWith (ExitFailure 1)
