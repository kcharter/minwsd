module Tokens where

import Control.DeepSeq (NFData(..))
import qualified Data.Text as T

data WordOrWS =
  Word { content :: T.Text } |
  WS { content :: T.Text }
  deriving (Eq, Ord, Show)

instance NFData WordOrWS where
  -- we're using strict text so I assume seq is sufficient
  rnf (Word t) = t `seq` ()
  rnf (WS s) = s `seq` ()
  
isWord :: WordOrWS -> Bool
isWord (Word _) = True
isWord (WS _) = False

isWS :: WordOrWS -> Bool
isWS (Word _) = False
isWS (WS _) = True

wordText :: WordOrWS -> Maybe T.Text
wordText (Word t) = Just t
wordText _ = Nothing
