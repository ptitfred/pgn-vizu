module Printer
    ( Locale(..)
    , printMatch
    , printMatchPositions
    , printBoard
    ) where

import           FEN                    (defaultBoard)
import           Models
import           Move

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, asks, runReaderT)
import           Data.List              (intersperse)

import           Text.Printf            (PrintfType, printf)

data Locale = English | French
data Context = Context { locale :: Locale }

type Printer = ReaderT Context IO

printMatch :: Locale -> Match -> IO ()
printMatch l m = runReaderT (matchPrinter m) (Context l)

printMatchPositions :: Locale -> Match -> IO ()
printMatchPositions l m = runReaderT (matchPositionPrinter m) (Context l)

printBoard :: Locale -> Board -> IO ()
printBoard l (Board position color castlingCapacities ep hm fm) = do
  mapM_ (putStrLn . map (showSquareContent l)) position
  printTurnToMove l fm color
  printCastlingCapacities l castlingCapacities
  printEnPassant l ep
  printHalfmovesClock l hm

printTurnToMove :: Locale -> Int -> Color -> IO ()
printTurnToMove English m c = printfLn "%i. %s to play"      m (showColor English c)
printTurnToMove French  m c = printfLn "%i. aux %s de jouer" m (showColor French  c)

printCastlingCapacities :: Locale -> [CastlingCapacity] -> IO ()
printCastlingCapacities English [] = putStrLn "nobody can castle"
printCastlingCapacities French  [] = putStrLn "personne ne peut roquer"
printCastlingCapacities l       cs = mapM_ (printCastlingCapacity l) cs

printCastlingCapacity :: Locale -> CastlingCapacity -> IO ()
printCastlingCapacity English (CanLongCastle  c) = printfLn "%s can O-O-O" $ showColor English c
printCastlingCapacity English (CanShortCastle c) = printfLn "%s can O-O"   $ showColor English c
printCastlingCapacity French  (CanLongCastle  c) = printfLn "les %s peuvent O-O-O" $ showColor French c
printCastlingCapacity French  (CanShortCastle c) = printfLn "les %s peuvent O-O"   $ showColor French c

printEnPassant :: Locale -> EnPassant -> IO ()
printEnPassant English (Just sq) = printfLn "%s can be taken en passant" $ showSquare sq
printEnPassant French  (Just sq) = printfLn "%s peut être pris en passant" $ showSquare sq
printEnPassant _        Nothing  = return ()

printHalfmovesClock :: Locale -> Int -> IO ()
printHalfmovesClock _ 0  = return ()
printHalfmovesClock English 1  = putStrLn "1 move since last capture or pawn move"
printHalfmovesClock English hm = printfLn "%i moves since last capture or pawn move" hm
printHalfmovesClock French  1  = putStrLn "1 coup depuis la dernière capture ou mouvement de pion"
printHalfmovesClock French  hm = printfLn "%i coups depuis la dernière capture ou mouvement de pion" hm

showSquareContent :: Locale -> SquareContent -> Char
showSquareContent _        Void                       = '.'
showSquareContent English (ColoredPiece Pawn   White) = 'P'
showSquareContent English (ColoredPiece Knight White) = 'N'
showSquareContent English (ColoredPiece Bishop White) = 'B'
showSquareContent English (ColoredPiece Rook   White) = 'R'
showSquareContent English (ColoredPiece Queen  White) = 'Q'
showSquareContent English (ColoredPiece King   White) = 'K'
showSquareContent English (ColoredPiece Pawn   Black) = 'p'
showSquareContent English (ColoredPiece Knight Black) = 'n'
showSquareContent English (ColoredPiece Bishop Black) = 'b'
showSquareContent English (ColoredPiece Rook   Black) = 'r'
showSquareContent English (ColoredPiece Queen  Black) = 'q'
showSquareContent English (ColoredPiece King   Black) = 'k'
showSquareContent French  (ColoredPiece Pawn   White) = 'P'
showSquareContent French  (ColoredPiece Knight White) = 'C'
showSquareContent French  (ColoredPiece Bishop White) = 'F'
showSquareContent French  (ColoredPiece Rook   White) = 'T'
showSquareContent French  (ColoredPiece Queen  White) = 'D'
showSquareContent French  (ColoredPiece King   White) = 'R'
showSquareContent French  (ColoredPiece Pawn   Black) = 'p'
showSquareContent French  (ColoredPiece Knight Black) = 'c'
showSquareContent French  (ColoredPiece Bishop Black) = 'f'
showSquareContent French  (ColoredPiece Rook   Black) = 't'
showSquareContent French  (ColoredPiece Queen  Black) = 'd'
showSquareContent French  (ColoredPiece King   Black) = 'r'

showColor :: Locale -> Color -> String
showColor English White = "white"
showColor English Black = "black"
showColor French  White = "blancs"
showColor French  Black = "noirs"

matchPrinter :: Match -> Printer ()
matchPrinter m = do
  localizedHeadersTitle <$> asks locale >>= putStrLnIO
  printHeaders $ matchHeaders m
  putStrLnIO ""
  localizedMovesTitle <$> asks locale >>= putStrIO
  printMove $ matchMoves m

matchPositionPrinter :: Match -> Printer ()
matchPositionPrinter m = do
  localizedHeadersTitle <$> asks locale >>= putStrLnIO
  printHeaders $ matchHeaders m
  putStrLnIO ""
  localizedMovesTitle <$> asks locale >>= putStrIO
  printMovePosition defaultBoard $ matchMoves m

localizedHeadersTitle :: Locale -> String
localizedHeadersTitle English = "Headers:"
localizedHeadersTitle French  = "En-têtes :"

localizedMovesTitle :: Locale -> String
localizedMovesTitle English = "Moves:"
localizedMovesTitle French  = "Coups :"

printHeaders :: Headers -> Printer ()
printHeaders = mapM_ printHeader

printHeader :: Header -> Printer ()
printHeader h = do
  headerText <- showHeaderLocalized h <$> asks locale
  putStrLnIO $ "  " ++ headerText

showHeaderLocalized :: Header -> Locale -> String
showHeaderLocalized (Event e)        English = "Event:        " ++ e
showHeaderLocalized (Site s)         English = "Site:         " ++ s
showHeaderLocalized (Date d)         English = "Date:         " ++ d
showHeaderLocalized (Round r)        English = "Round:        " ++ r
showHeaderLocalized (WhitePlayer wp) English = "White:        " ++ wp
showHeaderLocalized (BlackPlayer bp) English = "Black:        " ++ bp
showHeaderLocalized (Result rv)      English = "Result:       " ++ showResultValue rv English
showHeaderLocalized (WhiteElo elo)   English = "White Elo:    " ++ show elo
showHeaderLocalized (BlackElo elo)   English = "Black Elo:    " ++ show elo
showHeaderLocalized (PlyCount plies) English = "Ply count:    " ++ show plies
showHeaderLocalized (Variant v)      English = "Variant:      " ++ v
showHeaderLocalized (TimeControl tc) English = "Time control: " ++ tc
showHeaderLocalized (ECO eco)        English = "ECO:          " ++ eco
showHeaderLocalized (Opening opening)English = "Opening:      " ++ opening
showHeaderLocalized (Termination t)  English = "Termination:  " ++ t
showHeaderLocalized (Annotator an)   English = "Annotator:    " ++ an
showHeaderLocalized (Other k v)      English = k ++ ":  " ++ v
showHeaderLocalized (Event e)        French  = "Évènement :       " ++ e
showHeaderLocalized (Site s)         French  = "Site :            " ++ s
showHeaderLocalized (Date d)         French  = "Date :            " ++ d
showHeaderLocalized (Round r)        French  = "Ronde :           " ++ r
showHeaderLocalized (WhitePlayer wp) French  = "Blancs :          " ++ wp
showHeaderLocalized (BlackPlayer bp) French  = "Noirs :           " ++ bp
showHeaderLocalized (Result rv)      French  = "Résultat :        " ++ showResultValue rv French
showHeaderLocalized (WhiteElo elo)   French  = "Elo blancs :      " ++ show elo
showHeaderLocalized (BlackElo elo)   French  = "Elo noirs :       " ++ show elo
showHeaderLocalized (PlyCount plies) French  = "Nombre de coups : " ++ show plies
showHeaderLocalized (Variant v)      French  = "Variante :        " ++ v
showHeaderLocalized (TimeControl tc) French  = "Horloge :         " ++ tc
showHeaderLocalized (ECO eco)        French  = "ECO :             " ++ eco
showHeaderLocalized (Opening opening)French  = "Ouverture :       " ++ opening
showHeaderLocalized (Termination t)  French  = "Terminaison :     " ++ t
showHeaderLocalized (Annotator an)   French  = "Annoteur :        " ++ an
showHeaderLocalized (Other k v)      French  = k ++ " :  " ++ v

printBoardPosition :: Board -> Printer ()
printBoardPosition (Board p _ _ _ _ _) = do
  l <- asks locale
  mapM_ (putStrLnIO . map (showSquareContent l)) p

printMovePosition :: Board -> Move -> Printer ()
printMovePosition _ VariantEnd = return ()
printMovePosition _ (End result) = do
  putStrLnIO ""
  putStrIO "      "
  printResult result
printMovePosition board move@(HalfMove n c pm ch as nx _) = do
  let newBoard = moveBoard board move
  putStrLnIO ""
  putStrIO "  "
  printMoveNumber n
  case c of
    White -> return ()
    Black -> putStrIO "…"
  printPieceMove pm
  printCheck ch
  printAnnotations as
  putStrLnIO ""
  printBoardPosition newBoard
  case c of
    White -> if null as
             then putStrIO " "
             else do
               putStrLnIO ""
               putStrIO $ lpad (2 + 4 + 10 + 1 + 1) ""
    Black -> return ()
  putStrLnIO ""
  printMovePosition newBoard nx

printMove :: Move -> Printer ()
printMove VariantEnd = return ()
printMove (End result) = do
  putStrLnIO ""
  putStrIO "      "
  printResult result
printMove (HalfMove n c pm ch as nx vs) = do
  case c of
    White -> do
      putStrLnIO ""
      putStrIO "  "
      printMoveNumber n
    Black -> return ()
  printPieceMove pm
  printCheck ch
  printAnnotations as
  unless (null vs) $ printVariants vs
  case c of
    White -> if null as
             then putStrIO " "
             else do
               putStrLnIO ""
               putStrIO $ lpad (2 + 4 + 10 + 1 + 1) ""
    Black -> return ()
  printMove nx

printVariants :: [Move] -> Printer ()
printVariants []  = return ()
printVariants [_] = putStrIO " 1 variant"
printVariants vs  = putStrIO $ " " ++ show (length vs) ++ " variants"

printResult :: ResultValue -> Printer ()
printResult rv = showResultValue rv <$> asks locale >>= putStrLnIO

showResultValue :: ResultValue -> Locale -> String
showResultValue WhiteWins English = "white wins"
showResultValue BlackWins English = "black wins"
showResultValue Draw      English = "draw"
showResultValue Unknown   English = "uncertain"
showResultValue WhiteWins French  = "les blancs l'emportent"
showResultValue BlackWins French  = "les noirs l'emportent"
showResultValue Draw      French  = "nulle"
showResultValue Unknown   French  = "résultat incertain"

printMoveNumber :: Int -> Printer ()
printMoveNumber n = putStrIO $ lpad 4 (show n ++ ". ")

printPieceMove :: Action -> Printer ()
printPieceMove ShortCastle         = printFormattedPieceMove "O-O"
printPieceMove LongCastle          = printFormattedPieceMove "O-O-O"
printPieceMove (PieceMove p d c s) = do
  pieceText <- showPiece p
  printFormattedPieceMove $ pieceText ++ showDisambiguate d ++ showCapture c ++ showSquare s
printPieceMove (PawnMove c d p)  = do
  promotionText <- showPromotion p
  printFormattedPieceMove $ showCapture c ++ showSquare d ++ promotionText

printFormattedPieceMove :: String -> Printer ()
printFormattedPieceMove text = putStrIO (lpad 10 text)

showPiece :: Piece -> Printer String
showPiece p = showPieceLocalized p <$> asks locale

showPieceLocalized :: Piece -> Locale -> String
showPieceLocalized Pawn   English = "P"
showPieceLocalized Knight English = "N"
showPieceLocalized Bishop English = "B"
showPieceLocalized Rook   English = "R"
showPieceLocalized Queen  English = "Q"
showPieceLocalized King   English = "K"
showPieceLocalized Pawn   French  = "P"
showPieceLocalized Knight French  = "C"
showPieceLocalized Bishop French  = "F"
showPieceLocalized Rook   French  = "T"
showPieceLocalized Queen  French  = "D"
showPieceLocalized King   French  = "R"

showPromotion :: Promotion -> Printer String
showPromotion (PromoteTo p) = ("=" ++) <$> showPiece p
showPromotion  NoPromotion  = return ""

showCapture :: Capture -> String
showCapture  Capture            = "x"
showCapture (CaptureFromFile f) = showFile f ++ "x"
showCapture  NoCapture          = ""

showDisambiguate :: Disambiguate -> String
showDisambiguate (FileDisambiguate f)   = showFile f
showDisambiguate (RankDisambiguate r)   = showRank r
showDisambiguate (SquareDisambiguate s) = showSquare s
showDisambiguate  NoDisambiguate        = ""

showFile :: File -> String
showFile f = [f]

showRank :: Rank -> String
showRank r = [r]

showSquare :: Square -> String
showSquare (f,r) = [f,r]

printCheck :: Check -> Printer ()
printCheck None  = putStrIO " "
printCheck Check = putStrIO "+"
printCheck Mate  = putStrIO "#"

printAnnotations :: Annotations -> Printer ()
printAnnotations [] = return ()
printAnnotations as = do
  putStrIO " "
  sequence_ $ intersperse (putStrIO " ") $ map printAnnotation as

printAnnotation :: Annotation -> Printer ()
printAnnotation (GlyphAnnotation   g) = printGlyph g
printAnnotation (CommentAnnotation c) = printComment c

printGlyph :: Glyph -> Printer ()
printGlyph = putStrIO . showGlyph

showGlyph :: Glyph -> String
showGlyph (Glyph  1) = "!"
showGlyph (Glyph  2) = "?"
showGlyph (Glyph  3) = "!!"
showGlyph (Glyph  4) = "??"
showGlyph (Glyph  5) = "!?"
showGlyph (Glyph  6) = "?!"
showGlyph (Glyph 18) = "+/-"
showGlyph (Glyph 19) = "-/+"
showGlyph (Glyph  v) = "$" ++ show v

printComment :: Comment -> Printer ()
printComment c = do
  text <- showLocalizedComment c <$> asks locale
  putStrIO text

showLocalizedComment :: Comment -> Locale -> String
showLocalizedComment c English = "\"" ++ c ++ "\""
showLocalizedComment c French  = "« " ++ c ++ " »"

{- Utilities -}

lpad :: Int -> String -> String
lpad l s | l < 0     = error "padding size must be >= 0"
         | otherwise = pad ++ s'
  where l' = l - length s'
        s' = take l s
        pad = replicate l' ' '

putStrLnIO :: String -> Printer ()
putStrLnIO = liftIO . putStrLn

putStrIO :: String -> Printer ()
putStrIO = liftIO . putStr

printfLn :: PrintfType r => String -> r
printfLn p = printf (p ++ "\n")
