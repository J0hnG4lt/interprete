module Main (main) where

        --  Autor: Georvic Tur

import LexBot (Token(TkError), alexScanTokens)
import System.Environment
es_error :: Token -> Bool
es_error tok =
    case tok of
        (TkError _ _) -> True
        _ -> False

hay_error :: [Token] -> Bool
hay_error (x:s) = (es_error x) || (hay_error s)
hay_error [] = False


main :: IO ()
main = do
    [nombre] <- getArgs
    source <- readFile nombre
    let lista = alexScanTokens source
    if hay_error lista 
        then mapM_ (putStrLn . show) (filter es_error lista)
        else mapM_ (putStrLn . show) lista

