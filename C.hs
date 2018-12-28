-- | Support for parsing to tokens the source text of programs in C
-- and similar languages.

module C (
  cLanguage,
  shellLanguage,
  cLikeParser,
  cCommentParser,
  shellCommentParser) where

import Data.Char hiding (isPunctuation)

import Tokens
import Language

import Text.Regex.PCRE

-- | The language definition for C-like languages.
cLanguage :: Language
cLanguage = plainText { parser = cLikeParser cCommentParser}

-- | The language definition for shell-like languages.
shellLanguage :: Language
shellLanguage = plainText { parser = cLikeParser shellCommentParser }

-- | Parser for C-style tokens, but with a custom comment parser.
cLikeParser :: (String -> Maybe (Token, String)) -> String -> [Token]
cLikeParser _ [] = []
cLikeParser commentParser s =
  case (commentParser `alt` nonCommentParser) s of
    Nothing      -> [] -- TODO: this should probably be an error
    Just (t, s') -> t:cLikeParser commentParser s'

type Tokenizer = String -> Maybe (Token, String)

nonCommentParser :: Tokenizer
nonCommentParser [] = Nothing
nonCommentParser (c:rest)
  | c == '"'        = quoted '"' rest
  | c == '\''       = quoted '\'' rest
  | c == '.'        = afterDot rest
  | c == '0'        = afterZero rest
  | isDigit c       = afterDigit c rest
  | isSpace c       = whiteSpace c rest
  | isIdentStart c  = identifier c rest
  | otherwise       = oneCharWord c rest

alt :: Tokenizer -> Tokenizer -> Tokenizer
alt p q s =
  maybe (q s) Just (p s)

oneCharWord :: Char -> Tokenizer
oneCharWord c rest = Just (word [c], rest)

-- | Parser for C-like comments.
cCommentParser :: String -> Maybe (Token, String)
cCommentParser [] = Nothing
cCommentParser (c:rest)
  | c == '/'        =
    case rest of
      ('/':rest') -> oneLineComment "/" rest'
      ('*':rest') -> blockComment rest'
      _           -> oneCharWord c rest
  | otherwise       =
    Nothing

-- | Parser for shell-style one-line comments.
shellCommentParser :: String -> Maybe (Token, String)
shellCommentParser [] = Nothing
shellCommentParser (c:rest)
  | c == '#'  = oneLineComment "#" rest
  | otherwise = Nothing

oneLineComment :: String -> Tokenizer
oneLineComment start s =
  let (toEOL, rest) = span (not . startsEOL) s
      startsEOL c = c == '\r' || c == '\n'
  in Just (comment (start ++ toEOL), rest)

blockComment :: Tokenizer
blockComment s =
  accum "*/" s
  where accum sofar text =
          case text of
            '*':'/':rest -> Just (comment (reverse ('/':'*':sofar)), rest)
            '*':c:rest   -> accum ('*':sofar) (c:rest)
            c:rest       -> accum (c:sofar) rest
            []           ->
              -- the comment is terminated by the end of input; this is
              -- obviously garbage, but pretend it's a comment
              Just (comment (reverse sofar), [])

whiteSpace :: Char -> Tokenizer
whiteSpace c rest =
  let (spaces, rest') = span isSpace rest
  in Just (ws (c:spaces), rest')

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isAlpha c

identifier :: Char -> Tokenizer
identifier start rest =
  Just (word (start:identChars), rest')
  where (identChars, rest') = span isIdentChar rest
        isIdentChar c = c == '_' || isAlphaNum c

quoted :: Char -> Tokenizer
quoted q = accum [q]
  where accum sofar (c:rest)
          | c == q    = Just (word (reverse (q:sofar)), rest)
          | c == '\\' = case rest of
            d:rest' -> accum (d:c:sofar) rest'
            []      -> Just (word (reverse (c:sofar)), [])
          | otherwise = accum (c:sofar) rest
        accum sofar [] = Just (word (reverse sofar), [])

afterDot :: Tokenizer
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
     Just (word ('.':c:matched), after)
  | otherwise = Just (word ".", c:text)
afterDot [] =
  Just (word ".", [])

afterZero :: Tokenizer
afterZero text =
  let re = makeRegex "^[Xx]([0-9a-fA-F]+(\\.[0-9a-fA-F]*)?|\\.[0-9a-fA-F]+)([Pp][-+]?[0-9]+)?" :: Regex
      (_,matched,rest) = match re text :: (String,String,String)
  in case matched of
    [] -> afterDigit '0' text
    _  -> Just (word ('0':matched), rest)

afterDigit :: Char -> Tokenizer
afterDigit d text =
  let re = makeRegex "^([0-9]+(\\.[0-9]*)?|\\.[0-9]*)([Ee][-+]?[0-9]+)?" :: Regex
      (_,matched,rest) = match re text :: (String,String,String)
  in case matched of
    [] -> Just (word [d], text)
    _  -> Just (word (d:matched), rest)
