module Main where

import Data.Text
import Data.Text.IO
import Parser
import Prelude hiding (readFile, putStr, putStrLn, hPutStrLn)
import System.Environment
import System.Exit
import System.IO hiding (readFile, putStr, putStrLn, hPutStrLn)

main :: IO ()
main = do
  [drvFilenameA, drvFilenameB] <- getArgs
  drvString <- readFile drvFilenameA
  drv <- case parseAterm drvFilenameA (unpack drvString) of
    Left error -> printParseErrorAndExit error
    Right drv -> pure $ drv
  putStrLn $ pack (show drv)

printParseErrorAndExit :: ParseError -> IO a
printParseErrorAndExit error = do
  hPutStrLn stderr $ pack (show error)
  exitWith (ExitFailure 1)
