module MinWSDiff (minimizeWhitespaceDiffs, minimizeFileWhitespaceDiffs, minWSDiff) where

import Control.DeepSeq (deepseq)
import Control.Monad (liftM, liftM2)
import Data.Algorithm.Diff (Diff(..), getDiff)
import Data.Maybe (catMaybes)
import System.IO (Handle, withFile, hGetContents, IOMode(..))

import Language
import Tokens


-- | Given an old text and a new text, return a text with the new
-- words but whitespace and comments as similar as possible to the old
-- text.
minimizeWhitespaceDiffs :: Language -> String -> String -> String
minimizeWhitespaceDiffs lang oldText newText =
  unparse $ minWSDiff (parse oldText) (parse newText)
  where parse   = parser lang
        unparse = unparser lang

-- | Given an old file and a new file, read the files and return
-- text whose words are the same as in the new file, but whose
-- whitespace and comments match the old file as much as possible.
minimizeFileWhitespaceDiffs :: Language -> FilePath -> FilePath -> IO String
minimizeFileWhitespaceDiffs lang f1 f2 =
  unparse `liftM` liftM2 minWSDiff (tokenize f1) (tokenize f2)
  where tokenize = tokenizeFile (parser lang)
        unparse  = unparser lang

tokenizeFile :: (String -> [Token]) -> FilePath -> IO [Token]
tokenizeFile parse f =
  withFile f ReadMode (tokenizeHandle parse)

tokenizeHandle :: (String -> [Token]) -> Handle -> IO [Token]
tokenizeHandle parse h = do
  text <- hGetContents h
  let p = parse text in p `deepseq` return p

-- | Given old and new lists of tokens, produce a new list with the
-- new words, but with as much of the old whitespace and comments as
-- possible.
minWSDiff :: [Token] -> [Token] -> [Token]
minWSDiff old new =
  let diff = getDiff (wordTexts old) (wordTexts new)
      wordTexts = catMaybes . map wordText
      replaceWS diff oldNWs newNWs = replaceWS' [] diff oldNWs newNWs
      replaceWS' sofar diff oldNWs newNWs =
        case diff of
          (First _):diff' ->
            replaceWS' sofar diff' (tail oldNWs) newNWs
          (Second x):diff' ->
            -- we preserve both words and white space unique to the
            -- new stream; if it shows up in the diff, it is likely
            -- before a new word
            replaceWS' (Word x:(reverse (head newNWs) ++ sofar)) diff' oldNWs (tail newNWs)
          (Both x _):diff' ->
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
