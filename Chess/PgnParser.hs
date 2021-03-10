{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.PgnParser where

import Control.Monad

import Data.Functor
import Data.Maybe

import Text.Parsec
import Text.Parsec.String
import Data.Bifunctor

data CheckState = None | Check | Mate

type Square = (Char, Char)

data Move
    = Move 
      { turn :: Maybe Int
      , piece :: Char
      , rankSpec :: Maybe Char
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

{-
Tags:
    [ ... ]
Move:
  std:
    turn ?
    piece
    square_spec | rank_spec | file_spec ?
    captures ?
    destination
    promotion ?
    check | mate ?
  castles:
    turn ?
    O-O(-O)?
    check | mate ?
Comment:
    ; ... \n
    { ... }
Result:
   1/2-1/2 | 1-0 | 0-1
-}

tag :: Parser () 
tag = void $ between (char '[') (char ']') (many $ noneOf "]")

comment :: Parser ()
comment = lexeme (void (char ';' *> manyTill anyChar (void newline <|> eof)) <|> void (char '{' *> manyTill anyChar (char '}')))

move :: Parser Move
move = lexeme (try castles <|> stdMove)

lexeme p = p <* many (oneOf " \n\t")

rank = oneOf ['1'..'8']
file = oneOf ['a'..'h']

square = (,) <$> file <*> rank

turn' = optionMaybe (read <$> many1 digit <* char '.' <* many (char ' '))

piece' = oneOf "rnbkqpRNBKQP"

checkState = (char '+' $> Check) <|> (char '#' $> Mate) <|> return None

promotion' = optionMaybe (char '=' *> oneOf "rnbkqRNBKQ")

optionBool p = (p $> True) <|> return False

stdMove = do
    turn <- turn'
    piece <- piece'
    (rankSpec, fileSpec) <- (,) <$> optionMaybe rank <*> optionMaybe file
    captures <- optionBool (char 'x')
    if captures || isNothing rankSpec || isNothing fileSpec
        then do
            destination <- square
            promotion <- promotion'
            check <- checkState
            return $ Move { turn, piece, rankSpec, fileSpec, captures, destination, promotion, check }
        else do
            let destination = bimap fromJust fromJust (rankSpec, fileSpec)
            let (rankSpec, fileSpec) = (Nothing, Nothing)
            promotion <- promotion'
            check <- checkState
            return $ Move { turn, piece, rankSpec, fileSpec, captures, destination, promotion, check }

castles = do
    turn <- turn'
    string "O-O"
    long <- optionBool (string "-O")
    check <- checkState
    return $ Castles { turn, long, check }
    
