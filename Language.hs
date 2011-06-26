module Language where

import Tokens
import qualified Parse as P
import qualified Unparse as U

-- | Support for a particular language or family of languages.
data Language = Language {
  -- | Parses input text to a list of tokens.
  parser :: String -> [Token],
  -- | Unparses a list of tokens into text.
  unparser :: [Token] -> String
  }

-- | Default language support, equivalent to ordinary text
-- with no comments. For most programming languages, you
-- can simply replace the parser.
defaultLanguage :: Language
defaultLanguage = Language {
  parser = P.parse,
  unparser = U.unparse
  }
