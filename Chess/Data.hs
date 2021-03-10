module Chess.Data where

import Data.Char ( toLower, isAscii, isAsciiUpper )

type Piece = Char 

type Color = Bool

isPiece :: Char -> Bool
isPiece c = toLower c `elem` "bpknqr" && isAscii c

pieceColor :: Piece -> Color
pieceColor = isAsciiUpper

data CheckState = None | Check | Mate deriving (Show, Eq)

type Square = (Char, Char)

data Move
    = Move 
      { turn :: Maybe Int
      , piece :: Char
      , rankSpec :: Maybe Char
      , fileSpec :: Maybe Char
      , captures :: Bool
      , destination :: Square
      , check :: CheckState
      }
    | Push
      { turn :: Maybe Int
      , fileSpec :: Maybe Char
      , captures :: Bool
      , destination :: Square
      , promotion :: Maybe Char
      , check :: CheckState
      }
    | Castles
      { turn :: Maybe Int
      , long :: Bool
      , check :: CheckState
      }
    deriving (Show, Eq)