module Main where

import PGN

import Control.Monad (unless)
import Data.List (intersperse)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ("check" : files) = mapM checkFile files >>= exit
dispatch ("show"  : files) = sequence_ $ intersperse delimiter $ map showFile files
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
  r <- parseFile file
  case r of
    Right _ -> True  <$ putStrLn (file ++ " OK")
    Left  e -> False <$ putStrLn (file ++ " KO") <* putStr (asMessage e)
    where asMessage = indent . show
          indent = unlines . map ("  "++) . lines

showFile :: String -> IO ()
showFile f = do
  r <- parseFile f
  case r of
    Left  e -> print e
    Right m -> printMatch m

printMatch :: Match -> IO ()
printMatch m = do
  putStrLn "Headers:"
  mapM_ (indented 1) $ matchHeaders m
  putStrLn ""
  putStrLn "Moves:"
  mapM_ (\mv -> putStr "  " >> printMove mv) $ matchMoves m

printMove :: Move -> IO ()
printMove (Move number white black) = do
  printMoveNumber number
  printPly white
  putStr " "
  printPly black
  putStrLn ""
printMove (FinalMove number white black result) = do
  printMoveNumber number
  printMaybePly white
  putStr " "
  printMaybePly black
  putStrLn ""
  putStr " => "
  print result

printMoveNumber :: Int -> IO ()
printMoveNumber number = putStr $ lpad 4 (show number ++ ". ")

printMaybePly :: Maybe Ply -> IO ()
printMaybePly = printMaybe printPly

printPly :: Ply -> IO ()
printPly (Ply m) = putStr (lpad 11 m)
printPly (AnnotatedPly m g cs) = do
  putStr (lpad 11 m)
  printMaybeGlyph g
  unless (null cs) $ do
    putStr " « "
    putStr $ unwords cs
    putStr " »"

printMaybeGlyph :: Maybe Glyph -> IO ()
printMaybeGlyph = printMaybe printGlyph

printMaybe :: (a -> IO ()) -> Maybe a -> IO ()
printMaybe action (Just v) = action v
printMaybe _      Nothing  = return ()

printGlyph :: Glyph -> IO ()
printGlyph (Glyph v) = putStr (" $" ++ show v)

indented :: Show a => Int -> a -> IO ()
indented width o = do
  putStr $ take (width*2) $ repeat ' '
  print o

lpad :: Int -> String -> String
lpad l s | l < 0     = error "padding size must be >= 0"
         | otherwise = pad ++ s'
  where l' = l - length s'
        s' = take l s
        pad = replicate l' ' '
