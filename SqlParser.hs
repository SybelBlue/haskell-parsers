{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module SqlParser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Data.Functor
import Data.Char

import qualified Data.Map.Strict as M
import Data.Bifunctor
import Data.Either (fromRight)

{-
* query         =  select, ws, from, [ ws, join ], [ ws, where ] ;
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
    = ColId ColumnID
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

type ColumnID = (String, String)
type TableName = String
type Join = (TableName, ValueTest)
type From = (TableName, [Join])

data Query = Query { select :: [ColumnID], from :: From, joins :: [Join], wheres :: [ValueTest] } deriving Show

type Table = [M.Map String String]
type Database = M.Map String Table

database :: [(String, [[(String, String)]])] -> Database
database = M.fromList . map (second $ map M.fromList) 

queryP = do
    select <- selectP
    from <- fromP
    joins <- many joinP
    wheres <- many whereP
    return $ Query { select, from, joins, wheres }

selectP = symbol "SELECT" *> columnId `sepBy1` (char ',' <* many1 whitespace)

whereP = symbol "WHERE" *> valueTest

fromP = (,) <$> (symbol "FROM" *> tableName) <*> (joinP `sepBy` many1 whitespace)

joinP = (,) <$> (symbol "JOIN" *> tableName) <*> (symbol "on" *> valueTest)

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

getQuery :: String -> Query
getQuery s = case parse queryP "" s of
    Left e -> error (show e)
    Right v -> v

sqlEngine :: [(String,[[(String,String)]])] -> String -> [[(String,String)]]
sqlEngine = execute . database 

execute :: Database -> String -> [[(String,String)]]
execute db q = do
    let query = getQuery q
    undefined 


rawMovieDatabse = [ ( "movie"
                  , [ [ ( "id", "1" ), ( "name", "Avatar"   ), ( "directorID", "1" ) ]
                    , [ ( "id", "2" ), ( "name", "Titanic"  ), ( "directorID", "1" ) ]
                    , [ ( "id", "3" ), ( "name", "Infamous" ), ( "directorID", "2" ) ]
                    , [ ( "id", "4" ), ( "name", "Skyfall"  ), ( "directorID", "3" ) ]
                    , [ ( "id", "5" ), ( "name", "Aliens"   ), ( "directorID", "1" ) ]
                    ]
                  )
                , ( "actor"
                  , [ [ ( "id", "1" ), ( "name", "Leonardo DiCaprio" ) ]
                    , [ ( "id", "2" ), ( "name", "Sigourney Weaver"  ) ]
                    , [ ( "id", "3" ), ( "name", "Daniel Craig"      ) ]
                    ]
                  )
                , ( "director"
                  , [ [ ( "id", "1" ), ( "name", "James Cameron"   ) ]
                    , [ ( "id", "2" ), ( "name", "Douglas McGrath" ) ]
                    , [ ( "id", "3" ), ( "name", "Sam Mendes"      ) ]
                    ]
                  )
                , ( "actor_to_movie"
                  , [ [ ( "movieID", "1" ), ( "actorID", "2" ) ]
                    , [ ( "movieID", "2" ), ( "actorID", "1" ) ]
                    , [ ( "movieID", "3" ), ( "actorID", "2" ) ]
                    , [ ( "movieID", "3" ), ( "actorID", "3" ) ]
                    , [ ( "movieID", "4" ), ( "actorID", "3" ) ]
                    , [ ( "movieID", "5" ), ( "actorID", "2" ) ]
                    ]
                  )
                ]
