module Test where

import Data.Char
import qualified Data.Text as T
import Test.QuickCheck

import Tokens
import Parse
import Unparse
import MinWSDiff

main :: IO ()
main = do
  quickCheck prop_unparseParse
  quickCheck prop_minWSDiffXXIsX
  quickCheck prop_minWSDiffEmptyXIsX
  quickCheck prop_minWSDiffContainsJustNewWords

instance Arbitrary Token where
  arbitrary =
    oneof [(Word . T.pack) `fmap` (listOf1 $ arbitrary `suchThat` (not . isSpace)),
           (WS . T.pack) `fmap` (listOf1 $ elements " \t\r\n\f")]

prop_unparseParse :: [Token] -> Bool
prop_unparseParse wordsOrWs =
  minimize wordsOrWs == (parse . unparse) wordsOrWs

minimize :: [Token] -> [Token]
minimize (w1:w2:rest) =
  case w1 of
    Word t1 ->
      case w2 of
        Word t2 ->
          minimize (Word (T.append t1 t2):rest)
        WS _ ->
          w1:minimize (w2:rest)
    WS s1 ->
      case w2 of
        Word _ ->
          w1:minimize (w2:rest)
        WS s2 ->
          minimize $ WS (T.append s1 s2):rest
minimize s = s

prop_minWSDiffXXIsX :: [Token] -> Bool
prop_minWSDiffXXIsX wws = wws == minWSDiff wws wws

prop_minWSDiffEmptyXIsX :: [Token] -> Bool
prop_minWSDiffEmptyXIsX wws = wws == minWSDiff [] wws

prop_minWSDiffContainsJustNewWords :: [Token] -> [Token] -> Bool
prop_minWSDiffContainsJustNewWords old new =
  justWords new == justWords (minWSDiff old new)
  
justWords :: [Token] -> [Token]
justWords = filter isWord
