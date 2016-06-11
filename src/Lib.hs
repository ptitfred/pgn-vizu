module Lib
    ( Match(..)
    , Header(..)
    , ParseError
    , parseFile
    ) where

import Data.Maybe             (catMaybes)

import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)

parseFile :: FilePath -> IO (Either ParseError Match)
parseFile file = parseFromFile match file

data Match = Match { matchHeaders :: [Header]
                   } deriving (Show)

data Header = Event String
            | Site String
            | Date String
            | White String
            | Black String
            | Result String
            | WhiteElo Int
            | BlackElo Int
            | PlyCount String
            | Variant String
            | TimeControl String
            | ECO String
            | Opening String
            | Termination String
            | Annotator String
              deriving (Show)

parseHeader :: String -> String -> Maybe Header
parseHeader "Event" = Just . Event
parseHeader "Site" = Just . Site
parseHeader "Date" = Just . Date
parseHeader "White" = Just . White
parseHeader "Black" = Just . Black
parseHeader "Result" = Just . Result
parseHeader "WhiteElo" = Just . WhiteElo . read
parseHeader "BlackElo" = Just . BlackElo . read
parseHeader "PlyCount" = Just . PlyCount
parseHeader "Variant" = Just . Variant
parseHeader "TimeControl" = Just . TimeControl
parseHeader "ECO" = Just . ECO
parseHeader "Opening" = Just . Opening
parseHeader "Termination" = Just . Termination
parseHeader "Annotator" = Just . Annotator
parseHeader _ = const Nothing

match :: Parser Match
match = Match <$> headers <* body

body :: Parser [Char]
body = many anyChar <* eof

headers :: Parser [Header]
headers = catMaybes <$> many header <* newline

header :: Parser (Maybe Header)
header = char '[' *> headerContent <* char ']' <* newline

headerContent :: Parser (Maybe Header)
headerContent = do
  name  <- headerName
  _     <- char ' '
  value <- headerValue
  return $ parseHeader name value

headerName :: Parser String
headerName = many (noneOf " ")

headerValue :: Parser String
headerValue = char '"' *> many (noneOf "\"") <* char '"'
