module Main where

import Data.Text
import Data.Text.IO
import Derivation
import Parser
import Prelude hiding (readFile, putStr, putStrLn, hPutStrLn)
import System.Environment
import System.Exit
import System.IO hiding (readFile, putStr, putStrLn, hPutStrLn)

main :: IO ()
main = do
  [drvFilenameA, drvFilenameB] <- getArgs
  drvA <- filenameToDrv drvFilenameA
  drvB <- filenameToDrv drvFilenameB
  putStrLn $ pack (show drvA)

filenameToDrv :: String -> IO Derivation
filenameToDrv drvFilename = do
  drvString <- readFile drvFilename
  drvAterm <- case parseAterm drvFilename (unpack drvString) of
    Left error -> printErrorAndExit error
    Right aterm -> pure $ aterm
  case drvFromAterm drvAterm of
    Left error -> printErrorAndExit error
    Right drv -> pure $ drv

printErrorAndExit :: Show e => e -> IO a
printErrorAndExit error = do
  hPutStrLn stderr $ pack (show error)
  exitWith (ExitFailure 1)
