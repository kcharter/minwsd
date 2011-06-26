module Test where

import Data.Char
import qualified Data.Text as T
import Test.QuickCheck

import MinWSDiff
import Language
import Tokens

main :: IO ()
main = do
  quickCheck $ forAll (listOf plainTextTokens) $ prop_plainTextUnparseParse
  quickCheck prop_minWSDiffXXIsX
  quickCheck prop_minWSDiffEmptyXIsX
  quickCheck prop_minWSDiffContainsJustNewWords

instance Arbitrary Token where
  arbitrary =
    oneof [(Word . T.pack) `fmap` arbitrary,
           (WS . T.pack) `fmap` arbitrary,
           (Comment . T.pack) `fmap` arbitrary]

plainTextTokens :: Gen Token
plainTextTokens =
  oneof [(Word . T.pack) `fmap` (listOf1 $ arbitrary `suchThat` (not . isSpace)),
         (WS . T.pack) `fmap` (listOf1 $ elements " \t\r\n\f")]
  
prop_plainTextUnparseParse :: [Token] -> Bool
prop_plainTextUnparseParse wordsOrWs =
  minimizePlainText wordsOrWs == (parse . unparse) wordsOrWs
  where parse = parser defaultLanguage
        unparse = unparser defaultLanguage

minimizePlainText :: [Token] -> [Token]
minimizePlainText (w1:w2:rest) =
  case w1 of
    Word t1 ->
      case w2 of
        Word t2 ->
          minimizePlainText (Word (T.append t1 t2):rest)
        WS _ ->
          w1:minimizePlainText (w2:rest)
        Comment _ ->
          unexpectedComment
    WS s1 ->
      case w2 of
        Word _ ->
          w1:minimizePlainText (w2:rest)
        WS s2 ->
          minimizePlainText $ WS (T.append s1 s2):rest
        Comment _ ->
          unexpectedComment
    Comment _ ->
      unexpectedComment
    where unexpectedComment = error "There shouldn't be comments in 'plain text'."
minimizePlainText s = s

prop_minWSDiffXXIsX :: [Token] -> Bool
prop_minWSDiffXXIsX wws = wws == minWSDiff wws wws

prop_minWSDiffEmptyXIsX :: [Token] -> Bool
prop_minWSDiffEmptyXIsX wws = wws == minWSDiff [] wws

prop_minWSDiffContainsJustNewWords :: [Token] -> [Token] -> Bool
prop_minWSDiffContainsJustNewWords old new =
  justWords new == justWords (minWSDiff old new)
  
justWords :: [Token] -> [Token]
justWords = filter isWord
