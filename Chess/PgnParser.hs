{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Chess.PgnParser (pgn) where

import Chess.Data

import Control.Monad

import Data.Functor
import Data.Maybe

import Text.Parsec
import Text.Parsec.String
import Data.Bifunctor

tag :: Parser () 
tag = lexeme . void $ between (char '[') (char ']') (many $ noneOf "]")

pgn = many tag *> pgnBody

pgnBody = (comment *> pgnBody) 
      <|> ((eof <|> void (try result)) $> []) 
      <|> ((:) <$> move <*> pgnBody)

comment :: Parser ()
comment = lexeme (void (char ';' *> manyTill anyChar (void newline <|> eof)) <|> void (char '{' *> manyTill anyChar (char '}')))

move :: Parser Move
move = lexeme $ do
    turn <- turn'
    f <- castles <|> pawn <|> stdMove
    return (f turn)

result :: Parser String
result = choice $ string <$> ["1/2-1/2", "1-0", "0-1"]

lexeme p = p <* many (oneOf " \n\t")

rank = oneOf ['1'..'8']
file = oneOf ['a'..'h']

square = (,) <$> file <*> rank

turn' = optionMaybe (read <$> many1 digit <* char '.' <* many (char ' '))

piece' = oneOf "rnbkqRNBKQ"

checkState = (char '+' $> Check) <|> (char '#' $> Mate) <|> return None

promotion' = optionMaybe (char '=' *> piece')

captures' = optionBool (char 'x')

optionBool p = (p $> True) <|> return False

stdMove = do
    piece <- piece'
    (rankSpec, fileSpec) <- (,) <$> optionMaybe file <*> optionMaybe rank
    captures <- captures'
    if captures || isNothing rankSpec || isNothing fileSpec
        then do
            destination <- square
            check <- checkState
            return $ \turn -> Move { turn, piece, rankSpec, fileSpec, captures, destination, check }
        else do
            let destination = bimap fromJust fromJust (rankSpec, fileSpec)
            let (rankSpec, fileSpec) = (Nothing, Nothing)
            check <- checkState
            return $ \turn -> Move { turn, piece, rankSpec, fileSpec, captures, destination, check }

pawn = do
    file0 <- file
    captures <- captures'
    mDestFile <- if captures then Just <$> file else return Nothing
    rankSpec <- rank
    let destination = (fromMaybe file0 mDestFile, rankSpec)
    let fileSpec = file0 <$ mDestFile
    promotion <- promotion'
    check <- checkState
    return $ \turn -> Push { turn, fileSpec, captures, destination, promotion, check }

castles = do
    string "O-O"
    long <- optionBool (string "-O")
    check <- checkState
    return $ \turn -> Castles { turn, long, check }



{- SPEC
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
    dest_rank
    promotion ?
    check | mate ?
Comment:
    ; ... \n
    { ... }
Result:
   1/2-1/2 | 1-0 | 0-1
-}

