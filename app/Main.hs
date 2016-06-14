module Main where

import Lib

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ("check" : files) = mapM checkFile files >>= exit
dispatch ("show"  : files) = mapM_ showFile files
dispatch ("help"  : _    ) = help
dispatch ("-h"    : _    ) = help
dispatch ("--help": _    ) = help
dispatch (unknown : _    ) = putStrLn $ "Unknown action " ++ unknown
dispatch []                 = help

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
  r <- parseFile file
  case r of
    Right _ -> True  <$ putStrLn (file ++ " OK")
    Left  e -> False <$ putStrLn (file ++ " KO") <* putStr (asMessage e)
    where asMessage = indent . show
          indent = unlines . map ("  "++) . lines

showFile :: String -> IO ()
showFile f = parseFile f >>= print
