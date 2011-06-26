module Main where

import Control.Monad (unless)
import System.Environment
import System.Exit

import MinWSDiff

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 2) printUsageAndExit
  let f1:f2:_ = args
  minimizeFileWhitespaceDiffs f1 f2 >>= putStr

printUsageAndExit = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <old file> <new file>"
  exitFailure
  
