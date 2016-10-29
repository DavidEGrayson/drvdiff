module Main where

import Derivation
import Parser
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  [drvFilenameA, drvFilenameB] <- getArgs
  drvA <- filenameToDrv drvFilenameA
  drvB <- filenameToDrv drvFilenameB
  putStrLn $ show drvA

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
printErrorAndExit error = do
  hPutStrLn stderr $ show error
  exitWith (ExitFailure 1)
