module Language where

import Data.Char
import qualified Data.Text as T

import Tokens

-- | Support for a particular language or family of languages.
data Language = Language {
  -- | Parses input text to a list of tokens.
  parser :: String -> [Token],
  -- | Unparses a list of tokens into text.
  unparser :: [Token] -> String
  }

-- | Default language support, with the 'plainTextParse' parser and
-- the 'defaultUnparse' unparser. For most programming languages, you
-- can simply replace the parser.
defaultLanguage :: Language
defaultLanguage = Language {
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
             in (WS (T.pack (c:spaces)), s')
        else let (word, s') = break isSpace s
             in (Word (T.pack (c:word)), s')
  in w:plainTextParse s'

-- | Concatentates the contents of all the tokens, and inserts no
-- other characters. As long as your parser puts distributes all the
-- characters in the input among tokens, you can use this unparser.
defaultUnparse :: [Token] -> String
defaultUnparse = concatMap (T.unpack . content)
