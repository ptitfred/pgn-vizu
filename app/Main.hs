module Main where

import PGN
import Printer

import Control.Monad       (foldM)
import Data.List           (intersperse)
import Data.Maybe          (fromMaybe)
import System.Environment  (getArgs, lookupEnv)
import System.Exit         (ExitCode(..), exitSuccess, exitWith)

main :: IO ()
main = do
  locale <- getLocale
  args   <- getArgs
  dispatch locale args

dispatch :: Locale -> [String] -> IO ()
dispatch _      ("check" : files) = checkFiles files
dispatch locale ("show"  : files) = showFiles locale files
dispatch locale ("help"  : _    ) = help locale
dispatch locale ("-h"    : _    ) = help locale
dispatch locale ("--help": _    ) = help locale
dispatch locale (action  : _    ) = unknown locale action
dispatch locale []                = help locale

help :: Locale -> IO ()
help English = do
  putStrLn "Commands:"
  putStrLn " show files*  : show content of PGN files"
  newline
  putStrLn " check files* : attempt to parse PGN files"
  newline
  putStrLn " help         : this message"
help French = do
  putStrLn "Commandes:"
  putStrLn " show fichiers*  : affiche le contenu des fichiers PGN"
  newline
  putStrLn " check fichiers* : essaie de lire les fichiers PGN"
  newline
  putStrLn " help            : ce message"

checkFiles :: [String] -> IO ()
checkFiles files = mapM checkFile files >>= sumUpChecks

checkFile :: String -> IO Bool
checkFile file = do
  r <- parseFilePath file
  case r of
    Right _ -> True  <$ putStrLn (file ++ " OK")
    Left  e -> False <$ putStrLn (file ++ " KO") <* putStr (asMessage e)
    where asMessage = indent . show
          indent    = unlines . map ("  "++) . lines

sumUpChecks :: [Bool] -> IO ()
sumUpChecks = exit . length . filter not

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith (ExitFailure n)

showFiles :: Locale -> [String] -> IO ()
showFiles locale = sequence_ . nlSeparated . showAll
  where showAll     = map (showFile locale)
        nlSeparated = intersperse newline

showFile :: Locale -> String -> IO ()
showFile locale file = do
  result <- parseFilePath file
  case result of
    Right match      -> printMatch locale match
    Left  parseError -> print parseError

unknown :: Locale -> String -> IO ()
unknown English a = putStrLn $ "Unknown action " ++ a
unknown French  a = putStrLn $ "Action " ++ a ++ " inconnue"

getLocale :: IO Locale
getLocale = chooseLocale <$> getEnvs "en" ["LC_MESSAGES", "LANG"]

getEnvs :: String -> [String] -> IO String
getEnvs defaultLocale keys =
  fromMaybe defaultLocale <$> foldM firstToBeFound Nothing keys
    where firstToBeFound Nothing key = lookupEnv key
          firstToBeFound value   _   = return value

chooseLocale :: String -> Locale
chooseLocale locale =
  case shortLocale of
    "fr" -> French
    _    -> English
  where shortLocale = take 2 locale

newline :: IO ()
newline = putStrLn ""
