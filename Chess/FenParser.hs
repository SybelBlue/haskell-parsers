{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chess.FenParser (Fen(..), fen) where

import Chess.Data (Square, Color)

import Control.Monad

import Data.Functor

import Text.Parsec
import Text.Parsec.String

data Fen = Fen
    { board :: [[Either Int Char]]
    , toMove :: Color
    , castlingCodes :: [Char]
    , enPassantSq :: Maybe Square
    , halfmoveClock :: Int
    , turnClock :: Int
    } deriving (Show, Eq)

rank :: Parser [Either Int Char]
rank = many ((Left . read . pure <$> oneOf ['1'..'8']) <|> (Right <$> oneOf "rnbkqpRNBKQP"))

spaceBreak = many1 (char ' ')

dashable p = (char '-' $> mzero) <|> p

fen = do
    many (oneOf " \t\n")
    board <- (++) <$> count 7 (rank <* char '/') <*> (pure <$> rank)
    spaceBreak
    toMove <- (=='w') <$> oneOf "wb"
    spaceBreak
    castlingCodes <- dashable (many (oneOf "KQkq"))
    spaceBreak
    enPassantSq <- dashable (Just <$> ((,) <$> oneOf ['a'..'h'] <*> digit))
    spaceBreak
    halfmoveClock <- read <$> many1 digit
    spaceBreak
    turnClock <- read <$> many1 digit
    many (oneOf " \t\n")
    return $ Fen { board, toMove, castlingCodes, enPassantSq, halfmoveClock, turnClock }

