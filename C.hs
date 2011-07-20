-- | Support for parsing to tokens the source text of programs in C
-- and similar languages.

module C (cLanguage, cParser) where

import Data.Char hiding (isPunctuation)

import Tokens
import Language

import Text.Regex.PCRE

-- | The language definition for C-like languages.
cLanguage :: Language
cLanguage = plainText { parser = cParser }

-- | Parser for C-style tokens.
cParser :: String -> [Token]
cParser [] = []
cParser (c:rest)
  | c == '/'        =
    case rest of
      ('/':rest') -> oneLineComment rest'
      ('*':rest') -> blockComment rest'
      _           -> oneCharWord c rest
  | c == '"'        = quoted '"' rest
  | c == '\''       = quoted '\'' rest
  | c == '.'        = afterDot rest
  | c == '0'        = afterZero rest
  | isDigit c       = afterDigit c rest
  | isSpace c       = whiteSpace c rest
  | isIdentStart c  = identifier c rest
  | otherwise       = oneCharWord c rest

oneCharWord :: Char -> String -> [Token]
oneCharWord c rest = word [c]:cParser rest

oneLineComment :: String -> [Token]
oneLineComment s =
  let (toEOL, rest) = span (not . startsEOL) s
      startsEOL c = c == '\r' || c == '\n'
  in comment ('/':'/':toEOL):cParser rest

blockComment :: String -> [Token]
blockComment s =
  accum "*/" s
  where accum sofar text =
          case text of
            '*':'/':rest -> comment (reverse ('/':'*':sofar)):cParser rest
            '*':c:rest   -> accum ('*':sofar) (c:rest)
            c:rest       -> accum (c:sofar) rest
            []           ->
              -- the comment is terminated by the end of input; this is
              -- obviously garbage, but pretend it's a comment
              comment (reverse sofar):[]

whiteSpace :: Char -> String -> [Token]
whiteSpace c rest =
  let (spaces, rest') = span isSpace rest
  in ws (c:spaces):cParser rest'

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isAlpha c

identifier :: Char -> String -> [Token]
identifier start rest =
  word (start:identChars):cParser rest'
  where (identChars, rest') = span isIdentChar rest
        isIdentChar c = c == '_' || isAlphaNum c

quoted :: Char -> String -> [Token]
quoted q = accum [q]
  where accum sofar (c:rest)
          | c == q    = word (reverse (q:sofar)):cParser rest
          | c == '\\' = case rest of
            d:rest' -> accum (d:c:sofar) rest'
            []      -> word (reverse (c:sofar)):[]
          | otherwise = accum (c:sofar) rest
        accum sofar [] = word (reverse sofar):[]

afterDot :: String -> [Token]
afterDot (c:text)
  | isDigit c =
    -- We could simply put the digit into the regex, but a little
    -- experimentation shows that this is really slow for parsing
    -- languages like Java where the dot is very heavily used outside
    -- of numbers. Failing to match the regex is a *lot* slower than
    -- avoiding the regex most of the time by checking whether there
    -- is a digit after the dot. This makes me think that using
    -- regular expressions (even the regex-tdfa implementation) might
    -- not be such a hot idea.
    let re = makeRegex "^[0-9]*([Ee][-+]?[0-9]+)?" :: Regex
        (_,matched,after) = match re text :: (String,String,String)
    in -- since the regex can match the empty string, it never
       -- fails, and 'after' will be 'text' if 'matched' is empty
     word ('.':c:matched):cParser after
  | otherwise = word ".":cParser (c:text)
afterDot [] =
  word ".":[]

afterZero :: String -> [Token]
afterZero text =
  let re = makeRegex "^[Xx]([0-9a-fA-F]+(\\.[0-9a-fA-F]*)?|\\.[0-9a-fA-F]+)([Pp][-+]?[0-9]+)?" :: Regex
      (_,matched,rest) = match re text :: (String,String,String)
  in case matched of
    [] -> afterDigit '0' text
    _  -> word ('0':matched):cParser rest

afterDigit :: Char -> String -> [Token]
afterDigit d text =
  let re = makeRegex "^([0-9]+(\\.[0-9]*)?|\\.[0-9]*)([Ee][-+]?[0-9]+)?" :: Regex
      (_,matched,rest) = match re text :: (String,String,String)
  in case matched of
    [] -> word [d]:cParser text
    _  -> word (d:matched):cParser rest
