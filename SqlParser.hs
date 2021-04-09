{-# LANGUAGE FlexibleContexts #-}
module SqlParser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Data.Functor
import Data.Char

{-
query         =  select, ws, from, [ ws, join ], [ ws, where ] ;
* select        =  "SELECT ", column-id, [ { ", ", column-id } ] ;
* from          =  "FROM ", table-name, [ { ws, join } ] ;
* join          =  "JOIN ", table-name, " on ", value-test ;
* where         =  "WHERE ", value-test ;
* value-test    =  value, comparison, value;
* column-id     =  table-name, ".", column-name ;
* table-name    = ? a valid SQL table name ? ;
* column-name   = ? a valid SQL column name ? ;
* value         =  column-id | ? a number ? | ? a SQL single-quoted string ? ;
* comparison    =  " = " | " > " | " < " | " <= " | " >= " | " <> " ;
* ws            = " " | "\n" | ws, ws ;
-}

type ValueTest = (SQLValue, BinOp, SQLValue)

data SQLValue
    = ColId (String, String)
    | Quote String
    | Number Float
    deriving (Show, Eq)

data BinOp 
    = Eq
    | Lt
    | Gt
    | Le
    | Ge
    | Dm
    | Ne
    deriving (Show, Eq)

select = symbol "SELECT" *> columnName `sepBy1` (char ',' <* many1 whitespace)

whereP = symbol "WHERE" *> valueTest

joinP = (,) <$> (symbol "JOIN" *> tableName) <*> (symbol "on" *> valueTest)

fromP = (,) <$> (symbol "FROM" *> tableName) <*> (joinP `sepBy` many1 whitespace)

comparison :: Parser BinOp
comparison = char ' ' *> (
    (char '=' $> Eq) <|>
    (char '>' $> Lt) <|>
    (char '<' $> Gt) <|>
    (string "<=" $> Le) <|>
    (string ">=" $> Ge) <|>
    (string "<>" $> Ne)
    ) <* char ' '

valueTest :: Parser ValueTest
valueTest = (,,) <$> value <*> comparison <*> value

whitespace = oneOf " \n"

identifier :: Parser String
identifier = many1 (alphaNum <|> char '_') <* many whitespace

symbol s = uncasedString s <* many1 whitespace

tableName = identifier

columnName = identifier

columnId = (,) <$> identifier <*> (char '.' >> identifier)

value :: Parser SQLValue
value = (Number <$> try numeric) <|> (Quote <$> quoted) <|> (ColId <$> columnId)

-- thanks https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
quoted = char '"' *> (concat <$> many character) <* char '"'
  where character = (pure <$> nonEscape) <|> escape
        nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
        escape = (:) <$> char '\\' <*> ((:[]) <$> oneOf "\\\"0nrvtbf")

uncasedString :: String -> Parser String
uncasedString = foldr folder (return [])
  where folder c tail = ((:) <$> (char (toLower c) <|> char (toUpper c))) <*> tail

numeric = do
    head <- many1 digit
    tail <- ((:) <$> char '.' <*> many1 digit) <|> return []
    return $ read (head ++ tail)

sqlEngine :: [(String,[[(String,String)]])] -> String -> [[(String,String)]]
sqlEngine database = execute where
  execute :: String -> [[(String,String)]]
  execute query = undefined