module WordsAndWS where

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