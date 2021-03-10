{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chess.FenParser (Fen(..), fen) where

import Control.Monad

import Data.Functor

import Text.Parsec
import Text.Parsec.String

data Fen = Fen
    { board :: [[Either Int Char]]
    , toMove :: Char
    , castlingCodes :: [Char]
    , enPassant :: Maybe (Char, Char)
    , halfmove :: Int
    , turn :: Int
    } deriving (Show, Eq)

rank :: Parser [Either Int Char]
rank = many ((Left . read . pure <$> oneOf ['1'..'8']) <|> (Right <$> oneOf "rnbkqpRNBKQP"))

spaceBreak = many1 (char ' ')

dashable p = (char '-' $> mzero) <|> p

fen = do
    many (oneOf " \t\n")
    board <- rank `sepBy` char '/'
    spaceBreak
    toMove <- oneOf "wb"
    spaceBreak
    castlingCodes <- dashable (many (oneOf "KQkq"))
    spaceBreak
    enPassant <- dashable (Just <$> ((,) <$> oneOf ['a'..'h'] <*> digit))
    spaceBreak
    halfmove <- read <$> many1 digit
    spaceBreak
    turn <- read <$> many1 digit
    many (oneOf " \t\n")
    return $ Fen { board, toMove, castlingCodes, enPassant, halfmove, turn }

