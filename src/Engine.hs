module Engine
    ( move
    ) where

import Models

import Control.Monad (guard, (>=>))
import Data.List     (intersect)

type Error a = Either String a

move :: Move -> Board -> Error Board
move (HalfMove _ c a _ _ n _) = act c a >=> turn >=> move n
move  _                       = pure

turn :: Board -> Error Board
turn (Board p White cs ep h mn) = pure $ Board p Black cs ep h mn
turn (Board p Black cs ep h mn) = pure $ Board p White cs ep h (mn+1)

act :: Color -> Action -> Board -> Error Board
act White ShortCastle = m "e1g1" >=> m "h1f1"
act White LongCastle  = m "e1c1" >=> m "a1d1"
act Black ShortCastle = m "e8g8" >=> m "h8f8"
act Black LongCastle  = m "e8c8" >=> m "a8d8"
act c pm@(PieceMove _ _ Capture s) = locatePiece pm c >=> movePiece s >=> resetClock
act c pm@(PieceMove _ _ _ s)       = locatePiece pm c >=> movePiece s
act c (PawnMove cp s pr)     = pawnStart >=> putPawn s >=> enPassant >=> promote pr s c >=> resetClock
  where pawnStart = locatePawn s cp c
        putPawn s2 (b, s1) = flip (,) s1 <$> to s1 s2 b
        enPassant (b, s1) = if isEnpassant s1 s then enPassantModifier s1 s b else pure b

reachableSquares :: (Board, SquareContent) -> Error [Square]
reachableSquares (_, Void                     ) = pure []
reachableSquares (b, ColoredPiece Pawn   White) = pure []
reachableSquares (b, ColoredPiece Knight Black) = pure []
reachableSquares (b, ColoredPiece Bishop c    ) = pure []
reachableSquares (b, ColoredPiece Rook   c    ) = pure []
reachableSquares (b, ColoredPiece Queen  c    ) = pure []
reachableSquares (b, ColoredPiece King   c    ) = pure []

isPinned :: (Board, SquareContent) -> Error Bool

movePiece :: Square -> (Board, Square) -> Error Board
movePiece s2 (b, s1) = to s1 s2 b

isEnpassant :: Square -> Square -> Bool
isEnpassant (f1, '2') (f2, '4') = f1 == f2
isEnpassant (f1, '7') (f2, '5') = f1 == f2
isEnpassant _      _      = False

enPassantModifier :: Square -> Square -> Board -> Error Board
enPassantModifier (f, '2') (_, '4') (Board p c cs _ h mn) =
  pure $ Board p c cs (Just (f, '3')) h mn
enPassantModifier (f, '7') (_, '5') (Board p c cs _ h mn) =
  pure $ Board p c cs (Just (f, '6')) h mn
enPassantModifier _ _ b = pure b

resetClock :: Board -> Error Board
resetClock (Board p c cs ep _ mn)= pure $ Board p c cs ep 0 mn

m :: String -> Board -> Error Board
m [f1, r1, f2, r2] = s1 `to` s2
  where s1 = (f1, r1)
        s2 = (f2, r2)
m mt = const . fail $ "bad move command " ++ mt

locatePiece :: Action -> Color -> Board -> Error (Board, Square)
locatePiece (PieceMove p d _ s) c b = (,) b <$> findPiece b c p s d
locatePiece _                   _ _ = fail "locatePiece: bad action type"

findPiece :: Board -> Color -> Piece -> Square -> Disambiguate -> Error Square
findPiece board@(Board _ _ _ _ _ mn) color piece target d =
  disambiguate d piece mn $ filter (canReach piece target) $ findPieces board color piece

canReach :: Piece -> Square -> Square -> Bool
canReach Knight = validKnightMove
canReach Bishop = validBishopMove
canReach Rook   = validRookMove
canReach Queen  = validQueenMove
canReach King   = validKingMove
canReach Pawn   = const (const False)

validKnightMove :: Square -> Square -> Bool
validKnightMove s1 s2 = df == 1 && dr == 2 || df == 2 && dr == 1
  where (df, dr) = deltas s1 s2

validBishopMove :: Square -> Square -> Bool
validBishopMove s1 s2 = df == dr
  where (df, dr) = deltas s1 s2

validRookMove :: Square -> Square -> Bool
validRookMove (f1, r1) (f2, r2) = f1 == f2 || r1 == r2

validQueenMove :: Square -> Square -> Bool
validQueenMove s1 s2 = validBishopMove s1 s2 || validRookMove s1 s2

validKingMove :: Square -> Square -> Bool
validKingMove s1 s2 = (df == 0 || df == 1) && (dr == 0 || dr == 1)
  where (df, dr) = deltas s1 s2

deltas :: Square -> Square -> (Int, Int)
deltas (f1, r1) (f2, r2) = (df, dr)
  where df = delta f1 f2
        dr = delta r1 r2

delta :: Enum a => a -> a -> Int
delta e1 e2 = abs (fromEnum e1 - fromEnum e2)

disambiguate :: Disambiguate -> Piece -> Int -> [Square] -> Error Square
disambiguate _ _ _ [s] = pure s
disambiguate (FileDisambiguate f) _ _ ss = pure $ head $ filter (\(f', _) -> f == f') ss
disambiguate (RankDisambiguate r) _ _ ss = pure $ head $ filter (\(_, r') -> r == r') ss
disambiguate (SquareDisambiguate s) _ _ ss = pure $ head $ filter (== s) ss
disambiguate NoDisambiguate p mn ds = fail $ "disambiguate: unexpected disambiguate with NoDisambiguate and " ++ show (length ds) ++ " squares: " ++ show ds ++ " piece: " ++ showPiece p++ " move: " ++ show mn

showPiece :: Piece -> String
showPiece Pawn   = "pawn"
showPiece Knight = "knight"
showPiece Bishop = "bishop"
showPiece Rook   = "rook"
showPiece Queen  = "queen"
showPiece King   = "king"

findPieces :: Board -> Color -> Piece -> [Square]
findPieces (Board position _ _ _ _ _) color piece = pieces
  where pieces = map fst $ filter forPiece index
        index = indexed position
        forPiece (_, cp) = cp == ColoredPiece piece color

indexed :: [[SquareContent]] -> [(Square, SquareContent)]
indexed p = do
  f <- ['a'..'h']
  r <- ['1'..'8']
  let s = (f, r)
  let sc = getSquare s p
  guard (sc /= Void)
  return (s, sc)

getSquare :: Square -> [[SquareContent]] -> SquareContent
getSquare (f, r) = getFile f . getRank r

locatePawn :: Square -> Capture -> Color -> Board -> Error (Board, Square) -- TODO handle e2e4 e7e5...
locatePawn (_, r) (CaptureFromFile f) White b = pure (b, (f, pred r))
locatePawn (f, r)  _                  White b = pure (b, matchingPawn b White [(f, pred r), (f, pred (pred r))])
locatePawn (_, r) (CaptureFromFile f) Black b = pure (b, (f, succ r))
locatePawn (f, r)  _                  Black b = pure (b, matchingPawn b Black [(f, succ r), (f, succ (succ r))])

matchingPawn :: Board -> Color -> [Square] -> Square
matchingPawn (Board pos _ _ _ _ _) c sq = head $ sq `intersect` pawns
  where pawns = map fst $ filter (\(_, ColoredPiece p c') -> p == Pawn && c == c') $ indexed pos

promote :: Promotion -> Square -> Color -> Board -> Error Board
promote (PromoteTo p) s c = setPiece s . (\b -> (b, ColoredPiece p c))
promote  NoPromotion  _ _ = pure

to :: Square -> Square -> Board -> Error Board
to start end = getPiece start >=> setPiece end >=> removePiece start

getPiece :: Square -> Board -> Error (Board, SquareContent)
getPiece (file, rank) b@(Board position _ _ _ _ _) = pure (b, sc)
  where sc = getFile file . getRank rank $ position

setPiece :: Square -> (Board, SquareContent) -> Error Board
setPiece (f,r) (Board p c cs ep h mn, sc) = pure b'
  where p' = setRank r (setFile f (const sc)) p
        b' = Board p' c cs ep h mn

removePiece :: Square -> Board -> Error Board
removePiece (f, r) (Board p c cs ep h mn) = pure $ Board p' c cs ep h mn
  where p' = setRank r (setFile f (const Void)) p

getFile :: File -> [a] -> a
getFile 'a' = (!! 0)
getFile 'b' = (!! 1)
getFile 'c' = (!! 2)
getFile 'd' = (!! 3)
getFile 'e' = (!! 4)
getFile 'f' = (!! 5)
getFile 'g' = (!! 6)
getFile 'h' = (!! 7)
getFile  f  = error $ "getFile: bad file " ++ [f]

setFile :: File -> (a -> a) -> [a] -> [a]
setFile 'a' = setItem' 0
setFile 'b' = setItem' 1
setFile 'c' = setItem' 2
setFile 'd' = setItem' 3
setFile 'e' = setItem' 4
setFile 'f' = setItem' 5
setFile 'g' = setItem' 6
setFile 'h' = setItem' 7
setFile  f  = error $ "setFile: bad file " ++ [f]

setRank :: Rank -> (a -> a) -> [a] -> [a]
setRank '1' = setItem' 7
setRank '2' = setItem' 6
setRank '3' = setItem' 5
setRank '4' = setItem' 4
setRank '5' = setItem' 3
setRank '6' = setItem' 2
setRank '7' = setItem' 1
setRank '8' = setItem' 0
setRank  r  = error $ "setRank: bad rank " ++ [r]

setItem' :: Int -> (a -> a) -> [a] -> [a]
setItem' i f xs = take i xs ++ [f $ xs !! i] ++ drop (i+1) xs

getRank :: Rank -> [a] -> a
getRank '1' = (!! 7)
getRank '2' = (!! 6)
getRank '3' = (!! 5)
getRank '4' = (!! 4)
getRank '5' = (!! 3)
getRank '6' = (!! 2)
getRank '7' = (!! 1)
getRank '8' = (!! 0)
getRank  r  = error $ "getRank: bad rank " ++ [r]
