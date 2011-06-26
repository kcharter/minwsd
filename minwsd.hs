module Main where

import Control.DeepSeq (deepseq)
import Control.Monad (unless, liftM, liftM2)
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
  minimizeFileWhitespaceDiffs f1 f2 >>= putStr

minimizeFileWhitespaceDiffs :: FilePath -> FilePath -> IO String
minimizeFileWhitespaceDiffs f1 f2 =
  unparse `liftM` liftM2 minWSDiff (tokenizeFile f1) (tokenizeFile f2)
  
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
  
