{-# LANGUAGE NamedFieldPuns #-}
module Chess.Board where

import Data.Maybe ( fromMaybe )

import Chess.Data
import Chess.FenParser (Fen(..))
import Chess.PgnParser

type Board = [[Maybe Piece]]

fromFen :: Fen -> Board
fromFen Fen { board } = map (concatMap unpack) board
  where unpack (Left n) = replicate n Nothing
        unpack (Right c) = [Just c]

displayBoard :: Board -> String
displayBoard = (++) "  a b c d e f g h\n" . unlines . zipWith displayRank (reverse [1..8])
  where displayRank n r = show n ++ " " ++ unwords (map (pure . fromMaybe ' ') r)


