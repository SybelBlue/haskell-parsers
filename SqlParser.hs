module SqlParser where


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