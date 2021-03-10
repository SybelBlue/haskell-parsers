module Chess.Main where

import Chess.PgnParser 
import Chess.FenParser 
import Chess.Board

import Text.Parsec 
import Text.Parsec.String 

import System.Exit (exitFailure)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseF :: Parser a -> String -> IO a
parseF p fileName = parseFromFile p fileName >>= either report return
  where
    report err = do
        putStrLn $ "Error: " ++ show err
        exitFailure

main = putStrLn $
    let fenStr = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        prs = parseWithEof fen fenStr in
        either show (displayBoard . fromFen) prs

