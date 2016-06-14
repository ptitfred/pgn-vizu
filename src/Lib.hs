module Lib
    ( Match(..)
    , Move(..)
    , Ply(..)
    , Glyph(..)
    , Header(..)
    , ParseError
    , parseFile
    , variant
    ) where

import qualified Control.Applicative as A ((<|>))
import Data.Maybe (fromJust, isJust)
import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)

parseFile :: FilePath -> IO (Either ParseError Match)
parseFile file = parseFromFile match file

data Match = Match { matchHeaders :: [Header]
                   , matchMoves   :: [Move]
                   } deriving (Show)

data Header = Event String
            | Site String
            | Date String
            | Round String
            | White String
            | Black String
            | Result ResultValue
            | WhiteElo Int
            | BlackElo Int
            | PlyCount String
            | Variant String
            | TimeControl String
            | ECO String
            | Opening String
            | Termination String
            | Annotator String
            | Other String String
              deriving (Show)

data Move = Move Int Ply Ply
          | FinalMove Int (Maybe Ply) (Maybe Ply) ResultValue
           deriving (Show)

data Ply = Ply String
         | AnnotatedPly String (Maybe Glyph) [Comment]
           deriving (Show)

newtype Glyph = Glyph Int deriving (Show)
type Comment = String

data ResultValue = WhiteWins | BlackWins | Draw | Unknown deriving (Show)

readResultValue :: String -> ResultValue
readResultValue "1-0"     = WhiteWins
readResultValue "0-1"     = BlackWins
readResultValue "1/2-1/2" = Draw
readResultValue _         = Unknown

parseHeader :: String -> String -> Header
parseHeader "Event"       = Event
parseHeader "Site"        = Site
parseHeader "Date"        = Date
parseHeader "Round"       = Round
parseHeader "White"       = White
parseHeader "Black"       = Black
parseHeader "Result"      = Result . readResultValue
parseHeader "WhiteElo"    = WhiteElo . read
parseHeader "BlackElo"    = BlackElo . read
parseHeader "PlyCount"    = PlyCount
parseHeader "Variant"     = Variant
parseHeader "TimeControl" = TimeControl
parseHeader "ECO"         = ECO
parseHeader "Opening"     = Opening
parseHeader "Termination" = Termination
parseHeader "Annotator"   = Annotator
parseHeader h             = Other h

match :: Parser Match
match = Match <$> headers <*> moves <* eof

moves :: Parser [Move]
moves = spaces *> many move

move :: Parser Move
move = do
  moveNumber <- read <$> many1 digit <* char '.' <* spaces
  whitePly <- optionMaybe (ply moveNumber)
  r1 <- result <* spaces
  blackPly <- optionMaybe (try (ply moveNumber))
  r2 <- result <* spaces
  let r = r1 A.<|> r2
  case r of
    Nothing -> return $ Move moveNumber (fromJust whitePly) (fromJust blackPly)
    Just rv -> return $ FinalMove moveNumber whitePly blackPly rv

ply :: Int -> Parser Ply
ply moveNumber = do
  m <- many1 (oneOf "abcdefgh12345678NBRQKx+#=O-")
  g1 <- optionMaybe traditionalGlyph <* spaces
  g2 <- optionMaybe glyph <* spaces
  let g = g1 A.<|> g2
  c <- many (try (comment <* spaces))
  () <$ many (try variant <* spaces)
  () <$ optionMaybe (try (continuation moveNumber) <* spaces)
  if isJust g || not (null c)
  then return $ AnnotatedPly m g c
  else return $ Ply m

continuation :: Int -> Parser ()
continuation m = () <$ string (show m) <* string "..."

glyph :: Parser Glyph
glyph = (Glyph . read) <$> try (char '$' *> many1 digit)
     <|> traditionalGlyph

traditionalGlyph :: Parser Glyph
traditionalGlyph =
  tries
    [ "!!"  `toGlyph`  3
    , "??"  `toGlyph`  4
    , "!?"  `toGlyph`  5
    , "?!"  `toGlyph`  6
    , "!"   `toGlyph`  1
    , "?"   `toGlyph`  2
    , "+/-" `toGlyph` 16
    , "-/+" `toGlyph` 17
    ] -- TODO: support more plain text glyphes
    where toGlyph :: String -> Int -> Parser Glyph
          toGlyph text g = Glyph g <$ string text

variant :: Parser ()
variant = () <$ textBetween '(' ')'

tries :: [Parser a] -> Parser a
tries = choice . map try

comment :: Parser Comment
comment = char '{' *> spaces *> manyTill anyChar (try (spaces >> char '}'))

result :: Parser (Maybe ResultValue)
result = (fmap readResultValue) <$> optionMaybe (tries [string "1/2-1/2", string "1-0", string "0-1", string "*"])

textBetween :: Char -> Char -> Parser String
textBetween c1 c2 = between (char c1) (char c2) (content)
  where content = many (noneOf [c2])

headers :: Parser [Header]
headers = many header <* newline

header :: Parser (Header)
header = char '[' *> headerContent <* char ']' <* newline

headerContent :: Parser (Header)
headerContent = do
  name  <- headerName
  _     <- char ' '
  value <- headerValue
  return $ parseHeader name value

headerName :: Parser String
headerName = many (noneOf " ")

headerValue :: Parser String
headerValue = textBetween '"' '"'
