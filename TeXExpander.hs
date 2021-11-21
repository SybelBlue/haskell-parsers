{-# OPTIONS -fno-warn-unused-do-bind #-}
-- don't warn on unused parse captures
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE TupleSections #-}

import Text.Parsec
import Text.Parsec.Combinator (eof, sepEndBy)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)


parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (spaces *> p <* eof) ""

whitespaceChars :: [Char]
whitespaceChars = " \n\t"

data TeXToken = Ch Char | B [TeXToken] | C String deriving (Show, Eq)

command :: Parser String
command = char '\\' *> many alphaNum

block :: Parser [TeXToken]
block = between (char '{') (char '}') (many blockToken)

texToken' :: Parser Char -> Parser TeXToken
texToken' x = (C <$> command) <|> (B <$> block) <|> (Ch <$> x)

blockToken :: Parser TeXToken
blockToken = texToken' (noneOf "}")

texToken :: Parser TeXToken
texToken = texToken' anyChar

texLex = many texToken


