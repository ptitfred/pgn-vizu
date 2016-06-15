module PGN
    ( Match(..)
    , Move(..)
    , Color(..)
    , PieceMove(..)
    , Glyph(..)
    , Annotation(..)
    , Comment
    , Header(..)
    , ParseError
    , ResultValue(..)
    , parseFile
    ) where

import qualified Control.Applicative as A ((<|>))
import Data.Maybe (fromJust, isJust, fromMaybe)
import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)

parseFile :: FilePath -> IO (Either ParseError Match)
parseFile file = parseFromFile parseMatch file

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

data Move = HalfMove { moveNumber      :: Int
                     , moveColor       :: Color
                     , movePieceMove   :: PieceMove
                     , moveAnnotations :: [Annotation]
                     , moveNext        :: Move
                     , moveVariants    :: [Move]
                     }
          | End ResultValue
          | VariantEnd
           deriving (Show)

newtype PieceMove = PieceMove String deriving (Show)

data Annotation = GlyphAnnotation Glyph | CommentAnnotation Comment deriving (Show)
type Annotations = [Annotation]
newtype Glyph = Glyph Int deriving (Show)
type Comment = String

mkAnnotations :: Maybe Glyph -> [Comment] -> [Annotation]
mkAnnotations g cs = mkGlyphAnnotation g ++ mkCommentAnnotations cs
  where mkGlyphAnnotation Nothing   = []
        mkGlyphAnnotation (Just g') = [GlyphAnnotation g']
        mkCommentAnnotations = map CommentAnnotation

data ResultValue = WhiteWins | BlackWins | Draw | Unknown deriving (Show)

readResultValue :: String -> ResultValue
readResultValue "1-0"     = WhiteWins
readResultValue "0-1"     = BlackWins
readResultValue "1/2-1/2" = Draw
readResultValue _         = Unknown

readHeader :: String -> String -> Header
readHeader "Event"       = Event
readHeader "Site"        = Site
readHeader "Date"        = Date
readHeader "Round"       = Round
readHeader "White"       = WhitePlayer
readHeader "Black"       = BlackPlayer
readHeader "Result"      = Result . readResultValue
readHeader "WhiteElo"    = WhiteElo . read
readHeader "BlackElo"    = BlackElo . read
readHeader "PlyCount"    = PlyCount
readHeader "Variant"     = Variant
readHeader "TimeControl" = TimeControl
readHeader "ECO"         = ECO
readHeader "Opening"     = Opening
readHeader "Termination" = Termination
readHeader "Annotator"   = Annotator
readHeader h             = Other h

parseMatch :: Parser Match
parseMatch = Match <$> parseHeaders <*> parseFirstMove

parseFirstMove :: Parser Move
parseFirstMove = spaces *> parseMove 0 <* spaces <* eof

parseMove :: Int -> Parser Move
parseMove m = eithers [parseVariantEnd, parseResult] (parseHalfMove m)

parseResult :: Parser (Maybe Move)
parseResult = (fmap (End . readResultValue)) <$> optionMaybe parseResultValue
  where parseResultValue = tries [ string "1/2-1/2"
                                 , string "1-0"
                                 , string "0-1"
                                 , string "*"
                                 ]

parseVariantEnd :: Parser (Maybe Move)
parseVariantEnd = optionMaybe (VariantEnd <$ try (spaces *> char ')'))

parseHalfMove :: Int -> Parser Move
parseHalfMove previousNumber = do
  number      <- spaces *> parseMoveNumber previousNumber
  pieceMove   <- parsePieceMove
  annotations <- parseAnnotations
  variants    <- parseVariants number
  next        <- parseContinuation number *> spaces *> parseMove number
  return $ HalfMove { moveNumber      = number
                    , moveColor       = chooseColor (number > previousNumber)
                    , movePieceMove   = pieceMove
                    , moveAnnotations = annotations
                    , moveNext        = next
                    , moveVariants    = variants
                    }

chooseColor :: Bool -> Color
chooseColor True  = White
chooseColor False = Black

parseAnnotations :: Parser Annotations
parseAnnotations = do
  litteral <- optionMaybe parseLitteralGlyph <* spaces
  nag      <- optionMaybe parseGlyph <* spaces
  comments <- parseComments
  return $ mkAnnotations (litteral A.<|> nag) comments

parsePieceMove :: Parser PieceMove
parsePieceMove = PieceMove <$> many1 (oneOf "abcdefgh12345678NBRQKx+#=O-")

parseContinuation :: Int -> Parser ()
parseContinuation m = () <$ optionMaybe (try (string (show m) <* string "..."))

parseGlyph :: Parser Glyph
parseGlyph = parseNumericAnnotationGlyph <|> parseLitteralGlyph

parseNumericAnnotationGlyph :: Parser Glyph
parseNumericAnnotationGlyph = (Glyph . read) <$> try (char '$' *> many1 digit)

parseLitteralGlyph :: Parser Glyph
parseLitteralGlyph =
  tries
    [ "!!"   `toGlyph`  3
    , "??"   `toGlyph`  4
    , "!?"   `toGlyph`  5
    , "?!"   `toGlyph`  6
    , "!"    `toGlyph`  1
    , "?"    `toGlyph`  2
    , "+/-"  `toGlyph` 18
    , "-/+"  `toGlyph` 19
    , "±"    `toGlyph` 16
    , "\194" `toGlyph` 16
    ] -- TODO: support more plain text glyphes
    where toGlyph :: String -> Int -> Parser Glyph
          toGlyph text g = Glyph g <$ string text

parseVariants :: Int -> Parser [Move]
parseVariants m = many (try (parseVariant m) <* spaces)

parseVariant :: Int -> Parser Move
parseVariant m = char '(' *> parseContinuation m *> spaces *> parseMove m <* spaces

parseComments :: Parser [Comment]
parseComments = many (try (parseComment <* spaces))

parseComment :: Parser Comment
parseComment = char '{' *> spaces *> manyTill anyChar (try (spaces >> char '}'))

parseMoveNumber :: Int -> Parser Int
parseMoveNumber m = (fromMaybe m . fmap read) <$> optionMaybe (try (many1 digit <* char '.' <* spaces))

parseHeaders :: Parser [Header]
parseHeaders = many parseHeader <* newline

parseHeader :: Parser (Header)
parseHeader = char '[' *> parseHeaderContent <* char ']' <* newline

parseHeaderContent :: Parser (Header)
parseHeaderContent = readHeader <$> parseHeaderName <* spaces <*> parseHeaderValue

parseHeaderName :: Parser String
parseHeaderName = many (noneOf " ")

parseHeaderValue :: Parser String
parseHeaderValue = textBetween '"' '"'

{- Utilities -}

textBetween :: Char -> Char -> Parser String
textBetween c1 c2 = between (char c1) (char c2) (content)
  where content = many (noneOf [c2])

eithers :: [Parser (Maybe a)] -> Parser a -> Parser a
eithers [] main = main
eithers (alt:alts) main = do
  v <- alt
  if isJust v
  then return $ fromJust v
  else eithers alts main

tries :: [Parser a] -> Parser a
tries = choice . map try
