module Tokens where

import Control.DeepSeq (NFData(..))
import qualified Data.Text as T

data Token =
  Word { content :: T.Text } |
  WS { content :: T.Text } |
  Comment { content :: T.Text }
  deriving (Eq, Ord, Show)

instance NFData Token where
  -- we're using strict text so I assume seq is sufficient
  rnf (Word t) = t `seq` ()
  rnf (WS s) = s `seq` ()
  rnf (Comment c) = c `seq` ()
  
word :: String -> Token
word = Word . T.pack

ws :: String -> Token
ws = WS . T.pack

comment :: String -> Token
comment = Comment . T.pack

contentString :: Token -> String
contentString = T.unpack . content

isWord :: Token -> Bool
isWord (Word _) = True
isWord _ = False

isWS :: Token -> Bool
isWS (WS _) = True
isWS _ = False

isComment :: Token -> Bool
isComment (Comment _) = True
isComment _ = False

wordText :: Token -> Maybe T.Text
wordText (Word t) = Just t
wordText _ = Nothing 
