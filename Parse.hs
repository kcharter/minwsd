module Parse where

import Data.Char
import qualified Data.Text as T

import Tokens

parse :: String -> [WordOrWS]
parse [] = []
parse (c:s) =
  let (w, s') =
        if isSpace c
        then let (spaces, s') = span isSpace s
             in (WS (T.pack (c:spaces)), s')
        else let (word, s') = break isSpace s
             in (Word (T.pack (c:word)), s')
  in w:parse s'