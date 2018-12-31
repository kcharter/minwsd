module Main where

import Data.Char
import Test.QuickCheck

import C (cLanguage, shellLanguage)
import MinWSDiff
import Language
import Tokens

main :: IO ()
main = do
  quickCheck $ forAll (listOf plainTextTokens) $ prop_plainTextUnparseParse
  quickCheck prop_minWSDiffXXIsX
  quickCheck prop_minWSDiffEmptyXIsX
  quickCheck prop_minWSDiffContainsJustNewWords
  quickCheck $ gen_prop_parseUnparse plainText
  quickCheck $ gen_prop_parseUnparse cLanguage
  quickCheck $ gen_prop_parseUnparse shellLanguage

instance Arbitrary Token where
  arbitrary =
    oneof [word `fmap` arbitrary,
           ws `fmap` arbitrary,
           comment `fmap` arbitrary]

plainTextTokens :: Gen Token
plainTextTokens =
  oneof [word `fmap` (listOf1 $ arbitrary `suchThat` (not . isSpace)),
         ws `fmap` (listOf1 $ elements " \t\r\n\f")]

prop_plainTextUnparseParse :: [Token] -> Bool
prop_plainTextUnparseParse wordsOrWs =
  minimizePlainText wordsOrWs == (parse . unparse) wordsOrWs
  where parse = parser plainText
        unparse = unparser plainText

minimizePlainText :: [Token] -> [Token]
minimizePlainText (x1:x2:rest)
  | isWord x1 && isWord x2       = minimizePlainText (word (t1 ++ t2):rest)
  | isWS x1 && isWS x2           = minimizePlainText (ws (t1 ++ t2):rest)
  | isComment x1 || isComment x2 = unexpectedComment
  | otherwise                    = x1:minimizePlainText (x2:rest)
    where t1 = contentString x1
          t2 = contentString x2
          unexpectedComment = error "There shouldn't be comments in 'plain text'."
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

gen_prop_parseUnparse :: Language -> String -> Bool
gen_prop_parseUnparse lang input =
  input == (unparser lang $ parser lang $ input)
