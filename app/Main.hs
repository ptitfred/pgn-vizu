module Main where

import PGN
import Printer

import Data.List (intersperse)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ("check" : files) = mapM checkFile files >>= exit
dispatch ("show"  : files) = sequence_ $ intersperse delimiter $ map showFilePath files
dispatch ("help"  : _    ) = help
dispatch ("-h"    : _    ) = help
dispatch ("--help": _    ) = help
dispatch (unknown : _    ) = putStrLn $ "Unknown action " ++ unknown
dispatch []                 = help

delimiter :: IO ()
delimiter = putStrLn ""

exit :: [Bool] -> IO ()
exit checks =
  if failures == 0
  then exitSuccess
  else exitWith (ExitFailure failures)
    where failures = length $ filter not checks

help :: IO ()
help = do
  putStrLn "Commands:"
  putStrLn " show files*  : browse PGN files"
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

showFilePath :: String -> IO ()
showFilePath f = do
  r <- parseFilePath f
  case r of
    Left  e -> print e
    Right m -> printMatch m
