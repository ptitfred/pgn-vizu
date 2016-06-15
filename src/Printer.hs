module Printer
    ( printMatch
    ) where

import Models

import Control.Monad (unless)
import Data.List (intersperse)

printMatch :: Match -> IO ()
printMatch m = do
  putStrLn "Headers:"
  printHeaders $ matchHeaders m
  putStrLn ""
  putStr "Moves:"
  printMove $ matchMoves m

printHeaders :: Headers -> IO ()
printHeaders = mapM_ printHeader

printHeader :: Header -> IO ()
printHeader (Event e)         = putStrLn $ "  Event:        " ++ e
printHeader (Site s)          = putStrLn $ "  Site:         " ++ s
printHeader (Date d)          = putStrLn $ "  Date:         " ++ d
printHeader (Round r)         = putStrLn $ "  Round:        " ++ r
printHeader (WhitePlayer wp)  = putStrLn $ "  White:        " ++ wp
printHeader (BlackPlayer bp)  = putStrLn $ "  Black:        " ++ bp
printHeader (Result rv)       = putStrLn $ "  Result:       " ++ showResultValue rv
printHeader (WhiteElo elo)    = putStrLn $ "  White Elo:    " ++ show elo
printHeader (BlackElo elo)    = putStrLn $ "  Black Elo:    " ++ show elo
printHeader (PlyCount plies)  = putStrLn $ "  Ply count:    " ++ show plies
printHeader (Variant v)       = putStrLn $ "  Variant:      " ++ v
printHeader (TimeControl tc)  = putStrLn $ "  Time control: " ++ tc
printHeader (ECO eco)         = putStrLn $ "  ECO:          " ++ eco
printHeader (Opening opening) = putStrLn $ "  Opening:      " ++ opening
printHeader (Termination t)   = putStrLn $ "  Termination:  " ++ t
printHeader (Annotator an)    = putStrLn $ "  Annotator:    " ++ an
printHeader (Other k v)       = putStrLn $ "  " ++ k ++ ":  " ++ v

printMove :: Move -> IO ()
printMove VariantEnd = return ()
printMove (End result) = do
  putStrLn ""
  putStr "      "
  printResult result
printMove (HalfMove n c pm ch as nx vs) = do
  case c of
    White -> do
      putStrLn ""
      putStr "  "
      printMoveNumber n
    Black -> return ()
  printPieceMove pm
  printCheck ch
  printAnnotations as
  unless (null vs) $ do
    printVariants vs
  case c of
    White -> if null as
             then putStr " "
             else do
               putStrLn ""
               putStr $ lpad (2 + 4 + 10 + 1 + 1) ""
    Black -> return ()
  printMove nx

printVariants :: [Move] -> IO ()
printVariants []  = return ()
printVariants [_] = putStr " 1 variant"
printVariants vs  = putStr $ " " ++ (show $ length vs) ++ " variants"

printResult :: ResultValue -> IO ()
printResult = putStrLn . showResultValue

showResultValue :: ResultValue -> String
showResultValue WhiteWins = "white wins"
showResultValue BlackWins = "black wins"
showResultValue Draw      = "draw"
showResultValue _         = "uncertain"

printMoveNumber :: Int -> IO ()
printMoveNumber n = putStr $ lpad 4 (show n ++ ". ")

printPieceMove :: PieceMove -> IO ()
printPieceMove pm = putStr (lpad 10 (text pm))
  where text ShortCastle = "O-O"
        text LongCastle  = "O-O-O"
        text (PieceMove p d c s) = showPiece p ++ showDisambiguate d ++ showCapture c ++ showSquare s
        text (PawnMove f c d p) = showFile f ++ showCapture c ++ showSquare d ++ showPromotion p

showFile :: Maybe File -> String
showFile Nothing = ""
showFile (Just f) = [f]

showPiece :: Piece -> String
showPiece Knight = "N"
showPiece Bishop = "B"
showPiece Rook   = "R"
showPiece Queen  = "Q"
showPiece King   = "K"

showPromotion :: Promotion -> String
showPromotion (PromoteTo p) = "=" ++ showPiece p
showPromotion  NoPromotion  = ""

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

printCheck :: Check -> IO ()
printCheck None  = putStr " "
printCheck Check = putStr "+"
printCheck Mate  = putStr "#"

printAnnotations :: Annotations -> IO ()
printAnnotations [] = return ()
printAnnotations as = do
  putStr " "
  sequence_ $ intersperse (putStr " ") $ map printAnnotation as

printAnnotation :: Annotation -> IO ()
printAnnotation (GlyphAnnotation   g) = printGlyph g
printAnnotation (CommentAnnotation c) = printComment c

printGlyph :: Glyph -> IO ()
printGlyph = putStr . showGlyph

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

printComment :: Comment -> IO ()
printComment c = putStr $ "« " ++ c ++ " »"

{- Utilities -}

lpad :: Int -> String -> String
lpad l s | l < 0     = error "padding size must be >= 0"
         | otherwise = pad ++ s'
  where l' = l - length s'
        s' = take l s
        pad = replicate l' ' '
