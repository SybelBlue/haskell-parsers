{-# LANGUAGE NamedFieldPuns #-}
module Chess.Main where

import Data.Maybe

import Text.Parsec ( ParseError, parse )
import Text.Parsec.String ( Parser )

import Chess.PgnParser 
import Chess.FenParser 

import Data.Char ( toLower, isAscii, isAsciiUpper )

type Piece = Char 

type Color = Bool

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse p ""

isPiece :: Char -> Bool
isPiece c = toLower c `elem` "bpknqr" && isAscii c

pieceColor :: Piece -> Color
pieceColor = isAsciiUpper

type Board = [[Maybe Piece]]

fromFen :: Fen -> Board
fromFen Fen { board } = map (concatMap unpack) board
  where unpack (Left n) = replicate n Nothing
        unpack (Right c) = [Just c]

displayBoard :: Board -> String
displayBoard = (++) "  a b c d e f g h\n" . unlines . zipWith displayRank (reverse [1..8])
  where displayRank n r = show n ++ " " ++ unwords (map (pure . fromMaybe ' ') r)

main = putStrLn $
    let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        prs = parseWithEof fen fenStr in
        either show (displayBoard . fromFen) prs

