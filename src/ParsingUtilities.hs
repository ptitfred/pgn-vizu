{-# LANGUAGE FlexibleContexts #-}

module ParsingUtilities
    ( eithers
    , textBetween
    , tries
    , triesOr
    ) where

import Text.Parsec

tries :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
tries = choice . map try

triesOr :: Stream s m t => [ParsecT s u m a] -> a -> ParsecT s u m a
triesOr alternatives defaultValue = option defaultValue (tries alternatives)

textBetween :: Stream s m Char => Char -> Char -> ParsecT s u m String
textBetween c1 c2 = between (char c1) (char c2) content
  where content = many (noneOf [c2])

eithers :: Stream s m t => [ParsecT s u m (Maybe a)] -> ParsecT s u m a -> ParsecT s u m a
eithers []         main = main
eithers (alt:alts) main = alt >>= maybe (eithers alts main) return
