module Main where

import PGN
import Printer

import Control.Monad       (foldM)
import Data.List           (intersperse)
import Data.Maybe          (fromMaybe)
import System.Environment  (getArgs, lookupEnv)
import System.Exit         (ExitCode(..), exitSuccess, exitWith)
import Text.Printf         (PrintfType, printf)

main :: IO ()
main = do
  locale <- getLocale
  args   <- getArgs
  dispatch locale args

dispatch :: Locale -> [String] -> IO ()
dispatch _      ("check" : files) = checkFiles files
dispatch locale ("show"  : files) = showFiles locale files
dispatch locale ("board" : files) = showPositions locale files
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
  putStrLn " board files* : show content of PGN files with board positions"
  newline
  putStrLn " check files* : attempt to parse PGN files"
  newline
  putStrLn " help         : this message"
help French = do
  putStrLn "Commandes:"
  putStrLn " show fichiers*  : affiche le contenu des fichiers PGN"
  newline
  putStrLn " board fichiers* : affiche le contenu des fichiers PGN avec des positions sur échiquiers"
  newline
  putStrLn " check fichiers* : essaie de lire les fichiers PGN"
  newline
  putStrLn " help            : ce message"

checkFiles :: [FilePath] -> IO ()
checkFiles files = mapM checkFile files >>= sumUpChecks

checkFile :: FilePath -> IO Bool
checkFile file = parseFilePath file >>= either failure success
  where success matches = True  <$ printChecked file (length matches)
        failure e       = False <$ printNotChecked file e

printChecked :: FilePath -> Int -> IO ()
printChecked f 0  = printfLn "%s OK (no match)"   f
printChecked f 1  = printfLn "%s OK (1 match)"    f
printChecked f ms = printfLn "%s OK (%i matches)" f ms

printNotChecked :: FilePath -> ParseError -> IO ()
printNotChecked f e = do
  printfLn "%s KO" f
  putStr (asMessage e)
    where asMessage = indent . show
          indent    = unlines . map (printf "  %s") . lines

sumUpChecks :: [Bool] -> IO ()
sumUpChecks = exit . length . filter not

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith (ExitFailure n)

showFiles :: Locale -> [FilePath] -> IO ()
showFiles locale = mapLnM_ (showFile locale)

showFile :: Locale -> FilePath -> IO ()
showFile locale file = parseFilePath file >>= either print printMatches
  where printMatches = mapLnM_ (printMatch locale)

showPositions :: Locale -> [FilePath] -> IO ()
showPositions locale = mapLnM_ (showPosition locale)

showPosition :: Locale -> FilePath -> IO ()
showPosition locale file = parseFilePath file >>= either print printPositions
  where printPositions = mapLnM_ (printMatchPositions locale)

unknown :: Locale -> String -> IO ()
unknown English = printfLn "Unknown action '%s'"
unknown French  = printfLn "Action '%s' inconnue"

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

mapLnM_ :: (a -> IO ()) -> [a] -> IO ()
mapLnM_ action = sequence_ . intersperse newline . map action

newline :: IO ()
newline = putStrLn ""

printfLn :: PrintfType r => String -> r
printfLn p = printf (p ++ "\n")
