module PGN
    ( parseFilePath
    , ParseError
    ) where

import Models

import qualified Control.Applicative as A ((<|>))
import           Data.Maybe               (fromJust, isJust, fromMaybe)
import           Data.Text.IO             (readFile)
import           Prelude                  hiding (readFile)
import           Text.Parsec
import           Text.Parsec.Text         (Parser)

parseFilePath :: FilePath -> IO (Either ParseError Match)
parseFilePath file = parseFromFile parseMatch file

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile parser file = parse parser file <$> readFile file

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
  action      <- parseAction
  check       <- parseCheck
  annotations <- parseAnnotations
  variants    <- parseVariants number
  next        <- parseContinuation number *> spaces *> parseMove number
  return $ HalfMove { moveNumber      = number
                    , moveColor       = chooseColor (number > previousNumber)
                    , moveAction      = action
                    , moveCheck       = check
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

parseAction :: Parser Action
parseAction = tries [ parseLongCastle
                    , parseShortCastle
                    , parsePieceMove
                    , parsePawnMove
                    ]

parseLongCastle :: Parser Action
parseLongCastle = LongCastle <$ string "O-O-O"

parseShortCastle :: Parser Action
parseShortCastle = ShortCastle  <$ string "O-O"

parsePawnMove :: Parser Action
parsePawnMove =
  PawnMove <$> parseCapture
           <*> parseSquare
           <*> parsePromotion

parsePieceMove :: Parser Action
parsePieceMove = tries [ parseAmbiguousPieceMoveWithCapture
                       , parsePieceMoveWithCapture
                       , parseAmbiguousPieceMove
                       , parseSimplePieceMove
                       ]

parseAmbiguousPieceMoveWithCapture :: Parser Action
parseAmbiguousPieceMoveWithCapture =
  PieceMove <$> parsePiece
            <*> parseDisambiguate
            <*> parseCapture
            <*> parseSquare

parsePieceMoveWithCapture :: Parser Action
parsePieceMoveWithCapture =
  PieceMove <$> parsePiece
            <*> return NoDisambiguate
            <*> parseCapture
            <*> parseSquare

parseAmbiguousPieceMove :: Parser Action
parseAmbiguousPieceMove =
  PieceMove <$> parsePiece
            <*> parseDisambiguate
            <*> return NoCapture
            <*> parseSquare

parseSimplePieceMove :: Parser Action
parseSimplePieceMove =
  PieceMove <$> parsePiece
            <*> return NoDisambiguate
            <*> return NoCapture
            <*> parseSquare

parsePiece :: Parser Piece
parsePiece = tries [ Knight <$ char 'N'
                   , Bishop <$ char 'B'
                   , Rook   <$ char 'R'
                   , Queen  <$ char 'Q'
                   , King   <$ char 'K'
                   ]

parseDisambiguate :: Parser Disambiguate
parseDisambiguate = triesOr [ SquareDisambiguate <$> parseSquare
                            , FileDisambiguate   <$> parseFile
                            , RankDisambiguate   <$> parseRank
                            ] NoDisambiguate

parseSquare :: Parser Square
parseSquare = (,) <$> parseFile <*> parseRank

parseFile :: Parser File
parseFile = oneOf "abcdefgh"

parseRank :: Parser Rank
parseRank = oneOf "12345678"

parseCapture :: Parser Capture
parseCapture = triesOr [ CaptureFromFile <$> parseFile <* char 'x'
                       , Capture                       <$ char 'x'
                       ] NoCapture

parsePromotion :: Parser Promotion
parsePromotion = triesOr [ (PromoteTo Queen ) <$ string "=Q"
                         , (PromoteTo Rook  ) <$ string "=R"
                         , (PromoteTo Bishop) <$ string "=B"
                         , (PromoteTo Knight) <$ string "=N"
                         ] NoPromotion

parseCheck :: Parser Check
parseCheck = triesOr [ Check <$ char '+'
                     , Mate  <$ char '#'
                     ] None

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

triesOr :: [Parser a] -> a -> Parser a
triesOr alternatives defaultValue = option defaultValue (tries alternatives)
