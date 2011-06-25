module Tokens where

import Control.DeepSeq (NFData(..))
import qualified Data.Text as T

data Token =
  Word { content :: T.Text } |
  WS { content :: T.Text }
  deriving (Eq, Ord, Show)

instance NFData Token where
  -- we're using strict text so I assume seq is sufficient
  rnf (Word t) = t `seq` ()
  rnf (WS s) = s `seq` ()
  
isWord :: Token -> Bool
isWord (Word _) = True
isWord (WS _) = False

isWS :: Token -> Bool
isWS (Word _) = False
isWS (WS _) = True

wordText :: Token -> Maybe T.Text
wordText (Word t) = Just t
wordText _ = Nothing
