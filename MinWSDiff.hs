module MinWSDiff where

import Data.Algorithm.Diff
import Data.Maybe (catMaybes)

import Parse
import Unparse
import Tokens


-- | Given an old text and a new text, return a text with the new
-- words but whitespace as similar as possible to the old text.
minimizeWhitespaceDiffs :: String -> String -> String
minimizeWhitespaceDiffs oldText newText =
  unparse $ minWSDiff (parse oldText) (parse newText)
  
-- | Given and old stream of words and whitespace, and a new stream of
-- words and whitespace, produce a new stream with the new words in
-- the same order, but as much of the old whitespace as possible.
minWSDiff :: [WordOrWS] -> [WordOrWS] -> [WordOrWS]
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
nonWordGroups :: [WordOrWS] -> [[WordOrWS]]
nonWordGroups = foldr accum [[]]
  where accum x sofar
          | isWord x  = []:sofar
          | otherwise = (x:head sofar):tail sofar

