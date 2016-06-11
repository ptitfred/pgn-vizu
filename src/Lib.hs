module Lib
    ( Match(..)
    , parseFile
    ) where

import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)

parseFile :: FilePath -> IO (Either ParseError Match)
parseFile file = parseFromFile match file

data Match = Match { matchHeaders :: [Header]
                   } deriving (Show)

data Header = Header HeaderName String deriving (Show)

data HeaderName = Event
                | Site
                | Date
                | White
                | Black
                | Result
                | WhiteElo
                | BlackElo
                | PlyCount
                | Variant
                | TimeControl
                | ECO
                | Opening
                | Termination
                | Annotator
                  deriving (Show, Read)

match :: Parser Match
match = Match <$> headers <* body

body :: Parser [Char]
body = many anyChar <* eof

headers :: Parser [Header]
headers = many header <* newline

header :: Parser Header
header = char '[' *> headerContent <* char ']' <* newline

headerContent :: Parser Header
headerContent = do
  name  <- headerName
  _     <- char ' '
  value <- headerValue
  return $ Header name value

headerName :: Parser HeaderName
headerName = read <$> many (noneOf " ")

headerValue :: Parser String
headerValue = char '"' *> many (noneOf "\"") <* char '"'
