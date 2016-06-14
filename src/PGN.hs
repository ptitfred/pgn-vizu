module PGN
    ( Match(..)
    , Move(..)
    , Color(..)
    , Ply(..)
    , Glyph(..)
    , Annotation(..)
    , Comment
    , Header(..)
    , ParseError
    , ResultValue(..)
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
                   , matchMoves   :: Move
                   } deriving (Show)

data Header = Event String
            | Site String
            | Date String
            | Round String
            | WhitePlayer String
            | BlackPlayer String
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

data Color = White | Black deriving (Show)

data Move = Move { number   :: Int
                 , color    :: Color
                 , ply      :: Ply
                 , next     :: Move
                 , variants :: [Move]
                 }
          | End ResultValue
          | VariantEnd
           deriving (Show)

data Ply = Ply String [Annotation] deriving (Show)

data Annotation = GlyphAnnotation Glyph | CommentAnnotation Comment deriving (Show)

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
parseHeader "White"       = WhitePlayer
parseHeader "Black"       = BlackPlayer
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
match = Match <$> headers <*> (spaces *> move 0 <* spaces <* eof)

move :: Int -> Parser Move
move n = do
  r <- result
  if isJust r
  then return $ fromJust r
  else do
    spaces
    moveNumber <- optionMaybe $ try (many1 digit <* char '.' <* spaces)
    somePly <- ply'
    let color' = if isJust moveNumber then White else Black
    let n' = if isJust moveNumber then read (fromJust moveNumber) else n
    let variants' = []
    () <$ many (try variant <* spaces)
    () <$ optionMaybe (try (continuation n') <* spaces)
    spaces
    next' <- move n'
    return $ Move n' color' somePly next' variants'

ply' :: Parser Ply
ply' = do
  m <- many1 (oneOf "abcdefgh12345678NBRQKx+#=O-")
  glyph1 <- optionMaybe traditionalGlyph <* spaces
  glyph2 <- optionMaybe glyph <* spaces
  comments <- many (try (comment <* spaces))
  let glyph' = glyph1 A.<|> glyph2
  let annotations = mkAnnotations glyph' comments
  return $ Ply m annotations

mkAnnotations :: Maybe Glyph -> [Comment] -> [Annotation]
mkAnnotations g cs = mkGlyphAnnotation g ++ mkCommentAnnotations cs

mkGlyphAnnotation :: Maybe Glyph -> [Annotation]
mkGlyphAnnotation Nothing = []
mkGlyphAnnotation (Just g) = [GlyphAnnotation g]

mkCommentAnnotations :: [Comment] -> [Annotation]
mkCommentAnnotations = map CommentAnnotation

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

result :: Parser (Maybe Move)
result = (fmap (End . readResultValue)) <$> optionMaybe (tries [string "1/2-1/2", string "1-0", string "0-1", string "*"])

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
