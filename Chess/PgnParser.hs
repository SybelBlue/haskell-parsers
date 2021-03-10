{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.PgnParser where

import Control.Monad

import Data.Functor
import Data.Maybe

import Text.Parsec
import Text.Parsec.String
import Data.Bifunctor

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
    | Pawn
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

{-
Tags:
    [ ... ]
Move:
  std:
    piece
    square_spec | rank_spec | file_spec ?
    captures ?
    destination
    check | mate ?
  castles:
    O-O(-O)?
    check | mate ?
  pawn:
    file_spec
    (
        captures
        dest_file
    )?
    rank_spec
    promotion ?
    check | mate ?
Comment:
    ; ... \n
    { ... }
Result:
   1/2-1/2 | 1-0 | 0-1
-}
tag :: Parser () 
tag = lexeme . void $ between (char '[') (char ']') (many $ noneOf "]")

pgn = many tag *> sepEndBy (move <* optional comment) (many1 $ char ' ') <* (eof <|> void result)

comment :: Parser ()
comment = lexeme (void (char ';' *> manyTill anyChar (void newline <|> eof)) <|> void (char '{' *> manyTill anyChar (char '}')))

move :: Parser Move
move = lexeme (try castles <|> try pawn <|> stdMove)

result :: Parser String
result = choice $ string <$> ["1/2-1/2", "1-0", "0-1"]

lexeme p = p <* many (oneOf " \n\t")

rank = oneOf ['1'..'8']
file = oneOf ['a'..'h']

square = (,) <$> file <*> rank

turn' = optionMaybe (read <$> many1 digit <* char '.' <* many (char ' '))

piece' = oneOf "rnbkqRNBKQ"

checkState = (char '+' $> Check) <|> (char '#' $> Mate) <|> return None

promotion' = optionMaybe (char '=' *> oneOf "rnbkqRNBKQ")

captures' = optionBool (char 'x')

optionBool p = (p $> True) <|> return False

stdMove = do
    turn <- turn'
    piece <- piece'
    (rankSpec, fileSpec) <- (,) <$> optionMaybe rank <*> optionMaybe file
    captures <- optionBool (char 'x')
    if captures || isNothing rankSpec || isNothing fileSpec
        then do
            destination <- square
            check <- checkState
            return $ Move { turn, piece, rankSpec, fileSpec, captures, destination, check }
        else do
            let destination = bimap fromJust fromJust (rankSpec, fileSpec)
            let (rankSpec, fileSpec) = (Nothing, Nothing)
            check <- checkState
            return $ Move { turn, piece, rankSpec, fileSpec, captures, destination, check }

pawn = do
    turn <- turn'
    file0 <- file
    captures <- captures'
    mDestFile <- if captures then Just <$> file else return Nothing
    rankSpec <- rank
    let destination = (fromMaybe file0 mDestFile, rankSpec)
    let fileSpec = file0 <$ mDestFile
    promotion <- promotion'
    check <- checkState
    return $ Pawn { turn, fileSpec, captures, destination, promotion, check }

castles = do
    turn <- turn'
    string "O-O"
    long <- optionBool (string "-O")
    check <- checkState
    return $ Castles { turn, long, check }
    