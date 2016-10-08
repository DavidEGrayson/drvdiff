module Main where

import Parser ()
import Data.Text.IO (readFile, putStr)
import Prelude hiding (readFile, putStr)
import System.Environment (getArgs)

main :: IO ()
main = do
  [drvFilenameA, drvFilenameB] <- getArgs
  t <- readFile drvFilenameA
  putStr t
