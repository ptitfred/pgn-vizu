module Main where

import Lib

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch = mapM_ handleFile

handleFile :: String -> IO ()
handleFile f = parseFile f >>= print
