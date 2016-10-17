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
  drvString <- readFile drvFilenameA
  drvAterm <- case parseAterm drvFilenameA (unpack drvString) of
    Left error -> printErrorAndExit error
    Right aterm -> pure $ aterm
  drv <- case drvFromAterm drvAterm of
    Left error -> printErrorAndExit error
    Right drv -> pure $ drv
  putStrLn $ pack (show drv)

printErrorAndExit :: Show e => e -> IO a
printErrorAndExit error = do
  hPutStrLn stderr $ pack (show error)
  exitWith (ExitFailure 1)
