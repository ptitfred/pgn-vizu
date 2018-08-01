module Move
  (moveBoard) where

import           Models

import           Control.Lens hiding (Context)
import           Data.List    (elemIndex)
import           Data.Maybe   (fromMaybe, mapMaybe)

------------
-- Utilities
------------

fileList :: [File]
fileList = "abcdefgh"

rankList :: [Rank]
rankList = "87654321"

candidateSquaresFrom :: [Square]
candidateSquaresFrom = [(file, rank) | file <- fileList, rank <- rankList]

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

data MoveDistance = MoveDistance
  { fileDistance :: Int
  , rankDistance :: Int
  , taxiDistance :: Int
  }

----------------------------------
-- Board querying and manipulation
----------------------------------

opponentColor :: Color -> Color
opponentColor White = Black
opponentColor Black = White

squareCoords :: Square -> Maybe (Int, Int)
squareCoords (file, rank) = (,) <$> rankCoord <*> fileCoord
  where
    fileCoord = elemIndex file fileList
    rankCoord = elemIndex rank rankList

squareContents :: Position -> Square -> Maybe SquareContent
squareContents position square = case squareCoords square of
  Just (x, y) -> position ^? element x . element y
  Nothing     -> Nothing

updateBoard :: Square -> SquareContent -> Board -> Board
updateBoard square content (Board position co cc ep a b) = Board newPosition co cc ep a b
  where newPosition = case squareCoords square of
          Just (x, y) -> position & element x . element y .~ content
          Nothing     -> position

-- | Taking advantage of the fact that Product types are Functors on their last argument
squareBehind :: Color -> Square -> Square
squareBehind c s = case c of
  White -> pred <$> s
  Black -> succ <$> s

isPieceInSquare :: Piece -> Color -> Position -> Square -> Bool
isPieceInSquare piece color position square = case squareContents position square of
  Just contents -> contents == ColoredPiece piece color
  Nothing       -> False

isOpponentInSquare :: Color -> Position -> Square -> Bool
isOpponentInSquare color position square = case squareContents position square of
  Just (ColoredPiece _ c) -> c /= color
  _                       -> False

moveDistance :: Square -> Square -> MoveDistance
moveDistance (fromFile, fromRank) (toFile, toRank) =
  MoveDistance { fileDistance = fd
               , rankDistance = rd
               , taxiDistance = fd + rd
               }
  where
    fd = abs $ fromMaybe 0 $ subtract <$> elemIndex fromFile fileList <*> elemIndex toFile fileList
    rd = abs $ fromMaybe 0 $ subtract <$> elemIndex fromRank rankList <*> elemIndex toRank rankList

squaresBetween :: Square -> Square -> Maybe [Square]
squaresBetween fromSquare@(fromFile, fromRank) toSquare@(toFile, toRank)
  | fd <= 1 && rd <= 1 = Just []
  | fd == 0 && rd > 1  = Just [(fromFile, rank) | rank <- between fromRank toRank]
  | rd == 0 && fd > 1  = Just [(file, fromRank) | file <- between fromFile toFile]
  | fd == rd           = Just $ zip (between fromFile toFile) (between fromRank toRank)
  | otherwise          = Nothing
  where
    distance = moveDistance fromSquare toSquare
    fd = fileDistance distance
    rd = rankDistance distance
    between a b = if a > b then [pred a, (pred . pred) a .. succ b] else [succ a .. pred b]

---------------------------
-- Movement utilities
---------------------------

isMovingForward :: Color -> Square -> Square -> Bool
isMovingForward color (_, fromRank) (_, toRank) = case color of
  White -> fromRank < toRank
  Black -> fromRank > toRank

isValidPawnMoveSquareCount :: Color -> Square -> Square -> Bool
isValidPawnMoveSquareCount color (_, fromRank) (_, toRank) = case color of
  White -> succ fromRank == toRank || (fromRank == '2' && (succ . succ) fromRank == toRank)
  Black -> pred fromRank == toRank || (fromRank == '7' && (pred . pred) fromRank == toRank)

isPathClear :: Position -> Square -> Square -> Bool
isPathClear position fromSquare toSquare = case squaresBetween fromSquare toSquare of
  Just squares -> all (== Void) $ mapMaybe (squareContents position) squares
  Nothing      -> False

----------------------------
-- Movement validation
----------------------------

isValidPawnMove :: Color -> Capture -> Position -> Square -> Square -> Bool
isValidPawnMove color cap pos fromSquare toSquare = case cap of
  NoCapture           -> sensibleMove && noPiecesBetween
  (CaptureFromFile _) -> sensibleMove && (opponentInSquare || opponentPawnInEnPassantSquare)
  _                   -> False
  where
    sensibleMove = inSquare && movingForward && validSquareCount
    inSquare = isPieceInSquare Pawn color pos fromSquare
    movingForward = isMovingForward color fromSquare toSquare
    validSquareCount = isValidPawnMoveSquareCount color fromSquare toSquare
    noPiecesBetween = isPathClear pos fromSquare toSquare
    enPassantSquare = squareBehind color toSquare
    opponentPawnInEnPassantSquare = isPieceInSquare Pawn (opponentColor color) pos enPassantSquare
    opponentInSquare = isOpponentInSquare color pos toSquare

isValidPieceMove :: Action -> Color -> Board -> Square -> Bool
isValidPieceMove (PieceMove piece _ _ toSquare) color (Board position _ _ _ _ _) fromSquare = case piece of
    Knight -> sensibleMove && isValidKnightMove fromSquare toSquare
    Bishop -> sensibleMove && noPiecesBetween && isValidBishopMove fromSquare toSquare
    Rook   -> sensibleMove && noPiecesBetween && isValidRookMove fromSquare toSquare
    Queen  -> sensibleMove && noPiecesBetween && isValidQueenMove fromSquare toSquare
    King   -> sensibleMove && isValidKingMove fromSquare toSquare
    _      -> False
  where
    sensibleMove = inSquare && movingToDifferentSquare && not movingIntoOwnPiece
    inSquare = isPieceInSquare piece color position fromSquare
    movingToDifferentSquare = taxiDistance (moveDistance fromSquare toSquare) > 0
    movingIntoOwnPiece = isOpponentInSquare (opponentColor color) position toSquare
    noPiecesBetween = isPathClear position fromSquare toSquare
isValidPieceMove _ _ _ _ = False

isValidKnightMove :: Square -> Square -> Bool
isValidKnightMove fromSquare toSquare =
  taxiDistance distance == 3
  && all (< 3) [fileDistance distance, rankDistance distance]
  where distance = moveDistance fromSquare toSquare

isValidBishopMove :: Square -> Square -> Bool
isValidBishopMove fromSquare toSquare =
  fileDistance distance == rankDistance distance
  where distance = moveDistance fromSquare toSquare

isValidRookMove :: Square -> Square -> Bool
isValidRookMove fromSquare toSquare =
  (== 1) $ length $ filter (> 0) [fileDistance distance, rankDistance distance]
  where distance = moveDistance fromSquare toSquare

isValidQueenMove :: Square -> Square -> Bool
isValidQueenMove fromSquare toSquare =
  isValidRookMove fromSquare toSquare || isValidBishopMove fromSquare toSquare

isValidKingMove :: Square -> Square -> Bool
isValidKingMove fromSquare toSquare =
  all (<= 1) [fileDistance distance, rankDistance distance]
  where distance = moveDistance fromSquare toSquare

------------------
-- Piece discovery
------------------

findPawn :: Action -> Color -> Board -> Maybe Square
findPawn (PawnMove cap toSquare _) color (Board position _ _ _ _ _) =
  headMay
  $ filter (\fromSquare -> isValidPawnMove color cap position fromSquare toSquare)
  $ filter fromSameFile
  $ filter (isPieceInSquare Pawn color position) candidateSquaresFrom
  where
    fromSameFile (file, _) = file == fromFile
    fromFile = case cap of
      CaptureFromFile file -> file
      _                    -> fst toSquare
findPawn _ _ _ = Nothing

findPieceCandidates :: Action -> Color -> Board -> [Square]
findPieceCandidates action color board = filter (isValidPieceMove action color board) candidateSquaresFrom

disambiguatePieces :: Disambiguate -> [Square] -> Maybe Square
disambiguatePieces _ []               = Nothing
disambiguatePieces _ [square]         = Just square
disambiguatePieces d (square:squares) = case d of
  FileDisambiguate file -> if file == fst square then Just square else disambiguatePieces d squares
  RankDisambiguate rank -> if rank == snd square then Just square else disambiguatePieces d squares
  SquareDisambiguate sq -> if sq == square then Just square else disambiguatePieces d squares
  NoDisambiguate        -> Just square -- wat

-----------------------
-- Movement application
-----------------------

shortCastle :: Color -> Board -> Board
shortCastle color =
  updateBoard ('e', rank) Void
  . updateBoard ('h', rank) Void
  . updateBoard ('g', rank) (ColoredPiece King color)
  . updateBoard ('f', rank) (ColoredPiece Rook color)
  where rank = case color of
          White -> '1'
          Black -> '8'

longCastle :: Color -> Board -> Board
longCastle color =
  updateBoard ('a', rank) Void
  . updateBoard ('e', rank) Void
  . updateBoard ('c', rank) (ColoredPiece King color)
  . updateBoard ('d', rank) (ColoredPiece Rook color)
  where rank = case color of
          White -> '1'
          Black -> '8'

movePiece :: Square -> Square -> Board -> Board
movePiece fromSquare toSquare board@(Board position _ _ _ _ _) = clearFrom . moveTo $ board
  where
    clearFrom = updateBoard fromSquare Void
    moveTo = updateBoard toSquare pieceFrom
    pieceFrom = fromMaybe Void $ squareContents position fromSquare

movePawn :: Capture -> Promotion -> Color -> Square -> Square -> Board -> Board
movePawn cap prom color fromSquare toSquare board@(Board pos _ _ _ _ _) = case cap of
  CaptureFromFile _ -> clearFrom . moveTo . clearEnPassant $ board
  _                 -> clearFrom . moveTo $ board
  where
    clearFrom = updateBoard fromSquare Void
    moveTo = updateBoard toSquare coloredToPiece
    toPiece = case prom of
      PromoteTo p -> p
      NoPromotion -> Pawn
    coloredToPiece = ColoredPiece toPiece color
    enPassantSquare = squareBehind color toSquare
    opponentPawnOnEnPassantSquare = isPieceInSquare Pawn (opponentColor color) pos enPassantSquare
    clearEnPassant board' = case squareContents pos toSquare of
      Just (ColoredPiece _ _)  -> board'
      _                        -> if opponentPawnOnEnPassantSquare
                                  then updateBoard enPassantSquare Void board'
                                  else board'

moveBoard :: Board -> Move -> Board
moveBoard board (HalfMove _ color action _ _ _ _) = case action of
  ShortCastle                  -> shortCastle color board
  LongCastle                   -> longCastle color board
  (PieceMove _ d _ toSquare)   -> case fromSquare of
    Just sq -> movePiece sq toSquare board
    Nothing -> board
    where fromSquare = disambiguatePieces d $ findPieceCandidates action color board
  (PawnMove cap toSquare prom) -> case fromSquare of
    Just sq -> movePawn cap prom color sq toSquare board
    Nothing -> board
    where fromSquare = findPawn action color board
moveBoard board _ = board
