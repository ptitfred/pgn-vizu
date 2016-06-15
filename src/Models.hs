module Models
    ( Annotation(..)
    , Annotations
    , Capture(..)
    , Check(..)
    , Color(..)
    , Comment
    , Disambiguate(..)
    , File
    , Glyph(..)
    , Header(..)
    , Match(..)
    , Move(..)
    , Piece(..)
    , PieceMove(..)
    , Promotion(..)
    , Rank
    , ResultValue(..)
    , Square
    , mkAnnotations
    , readHeader
    , readResultValue
    ) where

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
                     , moveCheck       :: Check
                     , moveAnnotations :: Annotations
                     , moveNext        :: Move
                     , moveVariants    :: [Move]
                     }
          | End ResultValue
          | VariantEnd
           deriving (Show)

data PieceMove = ShortCastle
               | LongCastle
               | PieceMove Piece Disambiguate Capture Square
               | PawnMove { pawnStartFile   :: (Maybe File)
                          , pawnCapture     :: Capture
                          , pawnDestination :: Square
                          , pawnPromotion   :: Promotion
                          }
                 deriving (Show)

data Piece = Knight | Bishop | Rook | Queen | King deriving (Show)
type File = Char
type Rank = Char
type Square = (File, Rank)
data Disambiguate = FileDisambiguate File
                  | RankDisambiguate Rank
                  | SquareDisambiguate Square
                  | NoDisambiguate
                    deriving (Show)
data Capture = Capture | NoCapture deriving (Show)
data Promotion = PromoteTo Piece
               | NoPromotion
                 deriving Show
data Check = None | Check | Mate deriving (Show)

data Annotation = GlyphAnnotation Glyph | CommentAnnotation Comment deriving (Show)
type Annotations = [Annotation]
newtype Glyph = Glyph Int deriving (Show)
type Comment = String

data ResultValue = WhiteWins | BlackWins | Draw | Unknown deriving (Show)

mkAnnotations :: Maybe Glyph -> [Comment] -> Annotations
mkAnnotations g cs = mkGlyphAnnotation g ++ mkCommentAnnotations cs
  where mkGlyphAnnotation Nothing   = []
        mkGlyphAnnotation (Just g') = [GlyphAnnotation g']
        mkCommentAnnotations = map CommentAnnotation

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
