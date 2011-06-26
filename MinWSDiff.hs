module MinWSDiff (minimizeWhitespaceDiffs, minimizeFileWhitespaceDiffs, minWSDiff) where

import Control.DeepSeq (deepseq)
import Control.Monad (liftM, liftM2)
import Data.Algorithm.Diff
import Data.Maybe (catMaybes)
import System.IO (Handle, withFile, hGetContents, IOMode(..))

import Parse
import Unparse
import Tokens


-- | Given an old text and a new text, return a text with the new
-- words but whitespace as similar as possible to the old text.
minimizeWhitespaceDiffs :: String -> String -> String
minimizeWhitespaceDiffs oldText newText =
  unparse $ minWSDiff (parse oldText) (parse newText)

-- | Given an old file and a new file, read the files and return
-- text whose words are the same as in the new file, but whose
-- non-words match the old file as much as possible.
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

-- | Given and old stream of words and whitespace, and a new stream of
-- words and whitespace, produce a new stream with the new words in
-- the same order, but as much of the old whitespace as possible.
minWSDiff :: [Token] -> [Token] -> [Token]
minWSDiff old new =
  let diff = getDiff (wordTexts old) (wordTexts new)
      wordTexts = catMaybes . map wordText
      replaceWS diff oldNWs newNWs = replaceWS' [] diff oldNWs newNWs
      replaceWS' sofar diff oldNWs newNWs =
        case diff of
          (F, _):diff' ->
            replaceWS' sofar diff' (tail oldNWs) newNWs
          (S, x):diff' ->
            -- we preserve both words and white space unique to the
            -- new stream; if it shows up in the diff, it is likely
            -- before a new word
            replaceWS' (Word x:(reverse (head newNWs) ++ sofar)) diff' oldNWs (tail newNWs)
          (B, x):diff' ->
            replaceWS' (Word x:(reverse (head oldNWs) ++ sofar)) diff' (tail oldNWs) (tail newNWs)
          [] -> reverse $ (reverse (head newNWs) ++ sofar)
  in replaceWS diff (nonWordGroups old) (nonWordGroups new)


-- | A list of lists of non-words. All but the last are the possibly
-- empty consecutive non-words preceding a word. The last is the
-- possibly empty sequence of trailing non-words
nonWordGroups :: [Token] -> [[Token]]
nonWordGroups = foldr accum [[]]
  where accum x sofar
          | isWord x  = []:sofar
          | otherwise = (x:head sofar):tail sofar

