module Main where

import PGN
import Printer

import Control.Applicative ((<|>))
import Data.List           (intersperse)
import Data.Maybe          (fromMaybe)
import System.Environment  (getArgs, lookupEnv)
import System.Exit         (ExitCode(..), exitSuccess, exitWith)

main :: IO ()
main = do
  locale <- getLocale
  args   <- getArgs
  dispatch locale args

getLocale :: IO Locale
getLocale = do
  messages <- lookupEnv "LC_MESSAGES"
  lang <- lookupEnv "LANG"
  let locale = fromMaybe "en" $ messages <|> lang
  case (take 2 locale) of
    _    -> return English

dispatch :: Locale -> [String] -> IO ()
dispatch _      ("check" : files) = mapM checkFile files >>= exit
dispatch locale ("show"  : files) = sequence_ $ intersperse delimiter $ map (showFilePath locale) files
dispatch locale ("help"  : _    ) = help locale
dispatch locale ("-h"    : _    ) = help locale
dispatch locale ("--help": _    ) = help locale
dispatch locale (action  : _    ) = unknown locale action
dispatch locale []                = help locale

unknown :: Locale -> String -> IO ()
unknown English a = putStrLn $ "Unknown action " ++ a

delimiter :: IO ()
delimiter = putStrLn ""

exit :: [Bool] -> IO ()
exit checks =
  if failures == 0
  then exitSuccess
  else exitWith (ExitFailure failures)
    where failures = length $ filter not checks

help :: Locale -> IO ()
help English = do
  putStrLn "Commands:"
  putStrLn " show files*  : show content of PGN files"
  putStrLn ""
  putStrLn " check files* : attempt to parse PGN files"
  putStrLn ""
  putStrLn " help         : this message"

checkFile :: String -> IO Bool
checkFile file = do
  r <- parseFilePath file
  case r of
    Right _ -> True  <$ putStrLn (file ++ " OK")
    Left  e -> False <$ putStrLn (file ++ " KO") <* putStr (asMessage e)
    where asMessage = indent . show
          indent = unlines . map ("  "++) . lines

showFilePath :: Locale -> String -> IO ()
showFilePath l f = do
  r <- parseFilePath f
  case r of
    Left  e -> print e
    Right m -> printMatch l m
