module Main where

import Control.DeepSeq (deepseq)
import Control.Monad (unless)
import System.Environment
import System.Exit
import System.IO (Handle, withFile, hGetContents, IOMode(..))

import Parse
import Unparse
import MinWSDiff
import Tokens

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 2) printUsageAndExit
  let f1:f2:_ = args
  wws1 <- tokenizeFile f1
  wws2 <- tokenizeFile f2
  putStr $ unparse $ minWSDiff wws1 wws2

tokenizeFile :: FilePath -> IO [Token]
tokenizeFile f =
  withFile f ReadMode tokenizeHandle
  
tokenizeHandle :: Handle -> IO [Token]
tokenizeHandle h = do
  text <- hGetContents h
  let p = parse text in p `deepseq` return p

printUsageAndExit = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <old file> <new file>"
  exitFailure
  
