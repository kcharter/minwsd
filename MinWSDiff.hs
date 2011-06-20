module MinWSDiff where

import Data.Algorithm.Diff
import qualified Data.Text as T

import Parse
import Unparse
import WordsAndWS


-- | We use a newtype in order to impose an alternative equality
-- where all white space is considered equivalent.
newtype EqWS = EqWS WordOrWS deriving (Show)

instance Eq EqWS where
 (EqWS (WS _)) == (EqWS (WS _)) = True
 (EqWS w1) == (EqWS w2) = w1 == w2

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
  let diff = getDiff (toEqWS old) (toEqWS new)
      toEqWS = map EqWS
      replaceWS diff old new = replaceWS' [] diff old new
      replaceWS' sofar diff old new =
        case diff of
          (F, EqWS x):diff' ->
            case x of
              Word _ -> replaceWS' sofar diff' (dropExactly x old) new
              WS _   -> replaceWS' (x:sofar) diff' (dropExactly x old) new
          (S, EqWS x):diff' ->
            -- we preserve both words and white space unique to the
            -- new stream; if it shows up in the diff, it is likely
            -- before a new word
            replaceWS' (x:sofar) diff' old (dropExactly x new)
          (B, EqWS x):diff' ->
            case x of
              Word _ -> replaceWS' (x:sofar) diff' (dropExactly x old) (dropExactly x new)
              WS _   -> let (y, old') = extractLeadingWS old
                        in replaceWS' (y:sofar) diff' old' (dropExactly x new)
          [] -> reverse $ sofar
      dropExactly x (y:rest) =
        if x == y
        then rest
        else error $ "Expected '" ++ asString x ++ "', but got '" ++ asString y ++ "'." 
      dropExactly x [] =
        error $ "Expected '" ++ asString x ++ "', but list is empty."
      extractLeadingWS (y@(WS _):rest) = (y, rest)
      extractLeadingWS (x:rest) = error $ "Expected whitespace, but got '" ++ asString x ++ "'."
      extractLeadingWS [] = error $ "Expected whitespace, but list is empty."
      asString = T.unpack . content
  in replaceWS diff old new


-- | A list of pairs of zero-based word positions and lists of
-- preceding non-words.
precedingNonWords :: [WordOrWS] -> [(Int, [WordOrWS])]
precedingNonWords = foldr accum [] . zip [0..]
  where accum (i,x) sofar
          | isWord x  = (i,[]):sofar
          | otherwise = (case sofar of
                            [] -> []
                            ((i,nws):rest) -> ((i,x:nws):rest))

