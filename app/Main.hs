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
  putStr "Moves:"
  printMove $ matchMoves m

printMove :: Move -> IO ()
printMove VariantEnd = return ()
printMove (End result) = do
  putStrLn ""
  putStr "      "
  printResult result
printMove (Move n c p nx vs) = do
  case c of
    White -> do
      putStrLn ""
      putStr "  "
      printMoveNumber n
    Black -> return ()
  printPly p
  unless (null vs) $ do
    putStr $ (show $ length vs) ++ " variants"
  case c of
    White -> putStr " "
    Black -> return ()
  printMove nx

printResult :: ResultValue -> IO ()
printResult WhiteWins = putStrLn "white wins"
printResult BlackWins = putStrLn "black wins"
printResult Draw      = putStrLn "draw"
printResult _         = putStrLn "uncertain"

printMoveNumber :: Int -> IO ()
printMoveNumber n = putStr $ lpad 4 (show n ++ ". ")

printPly :: Ply -> IO ()
printPly (Ply m annotations) = do
  putStr (lpad 11 m)
  printAnnotations annotations

printAnnotations :: [Annotation] -> IO ()
printAnnotations [] = return ()
printAnnotations as = do
  putStr " "
  sequence_ $ intersperse (putStr " ") $ map printAnnotation as

printAnnotation :: Annotation -> IO ()
printAnnotation (GlyphAnnotation g)   = printGlyph g
printAnnotation (CommentAnnotation c) = printComment c

printGlyph :: Glyph -> IO ()
printGlyph (Glyph v) = putStr ("$" ++ show v)

printComment :: Comment -> IO ()
printComment c = putStr $ "« " ++ c ++ " »"

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
