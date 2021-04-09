module SqlParser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Data.Functor

{-
query         =  select, ws, from, [ ws, join ], [ ws, where ] ;
select        =  "SELECT ", column-id, [ { ", ", column-id } ] ;
from          =  "FROM ", table-name, [ { ws, join } ] ;
join          =  "JOIN ", table-name, " on ", value-test ;
where         =  "WHERE ", value-test ;
value-test    =  value, comparison, value;
column-id     =  table-name, ".", column-name ;
table-name    = ? a valid SQL table name ? ;
column-name   = ? a valid SQL column name ? ;
value         =  column-id | const
comparison    =  " = " | " > " | " < " | " <= " | " >= " | " <> " ;
const         =  ? a number ? | ? a SQL single-quoted string ? ;
ws            = " " | "\n" | ws, ws ;
-}

data Comparison 
    = Eq
    | Lt
    | Gt
    | Le
    | Ge
    | Dm
    | Ne
    deriving (Show, Eq)

comparison :: Parser Comparison
comparison = char ' ' *> (
    (char '=' $> Eq) <|>
    (char '>' $> Lt) <|>
    (char '<' $> Gt) <|>
    (string "<=" $> Le) <|>
    (string ">=" $> Ge) <|>
    (string "<>" $> Ne)
    ) <* char ' '


whitespace = void . many $ oneOf " \n"

identifier :: Parser String
identifier = many1 (alphaNum <|> char '_') <* whitespace

tableName = identifier

columnName = identifier

columnId = (,) <$> identifier <*> (char '.' >> identifier)
