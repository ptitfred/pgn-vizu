module Printer
    ( Locale(..)
    , printMatch
    ) where

import Models

import Control.Monad          (unless)
import Control.Monad.Reader   (ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import Data.List              (intersperse)

data Locale = English
data Context = Context { locale :: Locale }

type Printer = ReaderT Context IO

printMatch :: Locale -> Match -> IO ()
printMatch l m = runReaderT (matchPrinter m) (Context l)

matchPrinter :: Match -> Printer ()
matchPrinter m = do
  localizedHeadersTitle <$> asks locale >>= putStrLnIO
  printHeaders $ matchHeaders m
  putStrLnIO ""
  localizedMovesTitle <$> asks locale >>= putStrIO
  printMove $ matchMoves m

localizedHeadersTitle :: Locale -> String
localizedHeadersTitle English = "Headers:"

localizedMovesTitle :: Locale -> String
localizedMovesTitle English = "Moves:"

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
  unless (null vs) $ do
    printVariants vs
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
printVariants vs  = putStrIO $ " " ++ (show $ length vs) ++ " variants"

printResult :: ResultValue -> Printer ()
printResult rv = showResultValue rv <$> asks locale >>= putStrLnIO

showResultValue :: ResultValue -> Locale -> String
showResultValue WhiteWins English = "white wins"
showResultValue BlackWins English = "black wins"
showResultValue Draw      English = "draw"
showResultValue Unknown   English = "uncertain"

printMoveNumber :: Int -> Printer ()
printMoveNumber n = putStrIO $ lpad 4 (show n ++ ". ")

printPieceMove :: PieceMove -> Printer ()
printPieceMove ShortCastle         = printFormattedPieceMove "O-O"
printPieceMove LongCastle          = printFormattedPieceMove "O-O-O"
printPieceMove (PieceMove p d c s) = do
  pieceText <- showPiece p
  printFormattedPieceMove $ pieceText ++ showDisambiguate d ++ showCapture c ++ showSquare s
printPieceMove (PawnMove f c d p)  = do
  promotionText <- showPromotion p
  printFormattedPieceMove $ showFile f ++ showCapture c ++ showSquare d ++ promotionText

printFormattedPieceMove :: String -> Printer ()
printFormattedPieceMove text = putStrIO (lpad 10 text)

showFile :: Maybe File -> String
showFile Nothing = ""
showFile (Just f) = [f]

showPiece :: Piece -> Printer String
showPiece p = showPieceLocalized p <$> asks locale

showPieceLocalized :: Piece -> Locale -> String
showPieceLocalized Knight English = "N"
showPieceLocalized Bishop English = "B"
showPieceLocalized Rook   English = "R"
showPieceLocalized Queen  English = "Q"
showPieceLocalized King   English = "K"

showPromotion :: Promotion -> Printer String
showPromotion (PromoteTo p) = ("=" ++) <$> showPiece p
showPromotion  NoPromotion  = return ""

showCapture :: Capture -> String
showCapture NoCapture = ""
showCapture Capture   = "x"

showDisambiguate :: Disambiguate -> String
showDisambiguate (FileDisambiguate f)   = [f]
showDisambiguate (RankDisambiguate r)   = [r]
showDisambiguate (SquareDisambiguate s) = showSquare s
showDisambiguate NoDisambiguate         = ""

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
