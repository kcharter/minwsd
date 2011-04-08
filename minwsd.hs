module Main where

import Control.DeepSeq (deepseq)
import Control.Monad (unless)
import System.Environment
import System.Exit
import System.IO (withFile, hGetContents, IOMode(..))

import Parse
import Unparse
import MinWSDiff
import WordsAndWS

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 2) printUsageAndExit
  let f1:f2:_ = args
  wws1 <- getWordsAndWS f1
  wws2 <- getWordsAndWS f2
  putStr $ unparse $ minWSDiff wws1 wws2
  
getWordsAndWS :: FilePath -> IO [WordOrWS]
getWordsAndWS f =
  withFile f ReadMode $ \h -> do
    text <- hGetContents h
    let p = parse text in p `deepseq` return p
  
printUsageAndExit = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <old file> <new file>"
  exitFailure
  
