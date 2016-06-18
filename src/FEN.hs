module FEN
    ( Board(..)
    , FEN
    , ParseError
    , defaultBoard
    , parseBoard
    ) where

import Models
import ParsingUtilities

import Control.Monad      (guard)
import Data.Char          (digitToInt)
import Text.Parsec
import Text.Parsec.String (Parser)

type FEN = String

defaultBoard :: Board
defaultBoard = either undefined id $ parseBoard defaultFEN

parseBoard :: FEN -> Either ParseError Board
parseBoard = parse boardParser ""

defaultFEN :: String
defaultFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

boardParser :: Parser Board
boardParser = Board <$> parsePosition           <* space
                    <*> parseColor              <* space
                    <*> parseCastlingCapacities <* space
                    <*> parseEnPassant          <* space
                    <*> parseHalfmoveClock      <* space
                    <*> parseFullmoveNumber

parsePosition :: Parser Position
parsePosition = parseRow `sepBy` char '/'

parseRow :: Parser [SquareContent]
parseRow = do
  squares <- mconcat <$> many parseSquareContent
  guard (length squares == 8)
  return squares

parseSquareContent :: Parser [SquareContent]
parseSquareContent = tries [ [ColoredPiece Pawn   White] <$ char 'P'
                           , [ColoredPiece Knight White] <$ char 'N'
                           , [ColoredPiece Bishop White] <$ char 'B'
                           , [ColoredPiece Rook   White] <$ char 'R'
                           , [ColoredPiece Queen  White] <$ char 'Q'
                           , [ColoredPiece King   White] <$ char 'K'
                           , [ColoredPiece Pawn   Black] <$ char 'p'
                           , [ColoredPiece Knight Black] <$ char 'n'
                           , [ColoredPiece Bishop Black] <$ char 'b'
                           , [ColoredPiece Rook   Black] <$ char 'r'
                           , [ColoredPiece Queen  Black] <$ char 'q'
                           , [ColoredPiece King   Black] <$ char 'k'
                           , parseVoids
                           ]

parseVoids :: Parser [SquareContent]
parseVoids = do
  n <- digitToInt <$> digit
  guard (n >= 1 && n<= 8)
  return $ replicate n Void

parseColor :: Parser Color
parseColor = tries [ White <$ char 'w'
                   , Black <$ char 'b'
                   ]

parseCastlingCapacities :: Parser [CastlingCapacity]
parseCastlingCapacities = tries [ [] <$ char '-'
                                , many parseCastlingCapacity
                                ]

parseCastlingCapacity :: Parser CastlingCapacity
parseCastlingCapacity = tries [ CanLongCastle  White <$ char 'Q'
                              , CanShortCastle White <$ char 'K'
                              , CanLongCastle  Black <$ char 'q'
                              , CanShortCastle Black <$ char 'k'
                              ]

parseEnPassant :: Parser EnPassant
parseEnPassant = tries [ Nothing <$  char '-'
                       , Just    <$> parseEnPassantSquare
                       ]

parseEnPassantSquare :: Parser Square
parseEnPassantSquare = (,) <$> parseFile <*> oneOf "36"

parseFile :: Parser File
parseFile = oneOf "abcdefgh"

parseHalfmoveClock :: Parser Int
parseHalfmoveClock = parseInt

parseFullmoveNumber :: Parser Int
parseFullmoveNumber = parseInt

parseInt :: Parser Int
parseInt = read <$> many1 digit
