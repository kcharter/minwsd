module Language where

import Data.Char

import Tokens

-- | Support for a particular language or family of languages.
data Language = Language {
  -- | Parses input text to a list of tokens.
  parser :: String -> [Token],
  -- | Unparses a list of tokens into text.
  unparser :: [Token] -> String
  }

-- | A plain text language, with the 'plainTextParse' parser and the
-- 'defaultUnparse' unparser. This is a reasonable default language,
-- since for most programming languages you can simply replace the
-- parser.
plainText :: Language
plainText = Language {
  parser = plainTextParse,
  unparser = defaultUnparse
  }

-- | A plain text parser. Every non-empty sequence of non-whitespace
-- characters becomes a 'Word', and every non-empty sequence of
-- whitespace characters becomes a 'WS'. This parser never produces a
-- 'Comment'.
plainTextParse :: String -> [Token]
plainTextParse [] = []
plainTextParse (c:s) =
  let (w, s') =
        if isSpace c
        then let (spaces, s') = span isSpace s
             in (ws (c:spaces), s')
        else let (nonSpaces, s') = break isSpace s
             in (word (c:nonSpaces), s')
  in w:plainTextParse s'

-- | Concatentates the contents of all the tokens, and inserts no
-- other characters. As long as your parser puts distributes all the
-- characters in the input among tokens, you can use this unparser.
defaultUnparse :: [Token] -> String
defaultUnparse = concatMap contentString

-- | The type of next-token functions, which carve off the next token from the
-- input, if possible, yielding the token and the remainder of the input.
type NextToken = String -> Maybe (Token, String)

-- | Combines two next-token functions into one that tries the first and then
-- the second.
alt :: NextToken -> NextToken -> NextToken
alt p q s = maybe (q s) Just (p s)
