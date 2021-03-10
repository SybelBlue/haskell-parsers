{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Chess.Board where


import Data.Maybe ( fromMaybe )

import Chess.Data
import Chess.FenParser (Fen(..))
import Chess.PgnParser

data Board = Board 
    { rep :: [[Maybe Piece]]
    , enPassant :: Maybe Square
    , halfmove :: Int
    , turns :: Int
    , castling :: (Bool, Bool, Bool, Bool)
    , toPlay :: Color
    } deriving (Show, Eq)


fromFen :: Fen -> Board
fromFen Fen { board, enPassantSq , halfmoveClock, turnClock, toMove, castlingCodes } = 
    Board { rep, enPassant = enPassantSq, halfmove = halfmoveClock, turns = turnClock, toPlay = toMove, castling }
  where rep = map (concatMap unpack) board
        unpack (Left n) = replicate n Nothing
        unpack (Right c) = [Just c]
        castling = ('K' `elem` castlingCodes, 'Q' `elem` castlingCodes, 'k' `elem` castlingCodes, 'q' `elem` castlingCodes)

displayBoard :: Board -> String
displayBoard = (++) "  a b c d e f g h\n" . unlines . zipWith displayRank (reverse [1..8]) . rep
  where displayRank n r = show n ++ " " ++ unwords (map (pure . fromMaybe ' ') r)

makeMove :: Board -> Move -> Either String Board
makeMove Board { rep, enPassant, castling } = \case
    Castles { long } -> undefined 
    Push { fileSpec, captures, destination, promotion } -> undefined
    Move { piece, rankSpec, fileSpec, captures, destination } -> undefined
