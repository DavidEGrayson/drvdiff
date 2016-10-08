module Main where

import Data.Text
import Data.Text.IO
import Parser
import Prelude hiding (readFile, putStr)
import System.Environment
import System.Exit

main :: IO ()
main = do
  [drvFilenameA, drvFilenameB] <- getArgs
  drvString <- readFile drvFilenameA
  drv <- case parseDrv (unpack drvString) of
    Left error -> printParseErrorAndExit error
    Right drv -> pure $ drv
  putStr drvString

printParseErrorAndExit :: ParseError -> IO a
printParseErrorAndExit error = do
  -- TODO: print (formatParseError error)
  exitWith (ExitFailure 1)
