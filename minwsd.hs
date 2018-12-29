module Main where

import Data.List (intercalate, sort)
import qualified Data.Map.Strict as DM
import System.Exit

import Options.Applicative
import Data.Semigroup ((<>))

import C
import Language (Language())
import MinWSDiff

data Options = Options {
  language :: String,
  oldFile :: String,
  newFile :: String
}

options :: Parser Options
options = Options
  <$> strOption
      (  long "language"
      <> short 'l'
      <> value "c"
      <> metavar "LANGUAGE"
      <> help "Source language")
  <*> (  argument str (metavar "OLD_FILE"))
  <*> (  argument str (metavar "NEW_FILE"))

supportedLanguages :: DM.Map String Language
supportedLanguages =
  DM.fromList [
    ("c", cLanguage),
    ("c++", cLanguage),
    ("java", cLanguage),
    ("shell", shellLanguage)
  ]

namedLanguage :: String -> Either String Language
namedLanguage n =
  maybe (Left noSuchLanguage) (Right) $ DM.lookup n supportedLanguages
  where noSuchLanguage = "'" ++ n ++ "' isn't supported. Supported languages are " ++ nameSupported
        nameSupported  = intercalate ", " $ quoteAll $ sort $ DM.keys supportedLanguages
        quoteAll = map (\s -> "'" ++ s ++ "'")

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      (  fullDesc
      <> progDesc "Makes whitespace and comments in NEW_FILE like those in OLD_FILE" )

run :: Options -> IO ()
run opts =
  either failWithErrorMsg runWithLanguage (namedLanguage $ language opts)
  where failWithErrorMsg msg =
          putStrLn msg >> exitFailure
        runWithLanguage l =
          minimizeFileWhitespaceDiffs l (oldFile opts) (newFile opts) >>= putStr
