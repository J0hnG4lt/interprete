module Main (main) where

        -- Autor: Georvic Tur
        -- Carnet: 12-11402
        -- alexanderstower@gmail.com

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

{--
    Si no hay ningun error, se aplica mapM_ a toda la lista de tokens. 
    De lo contrario, se filtra la lista de tokens y sólo se imprimen
    los tokens de error.
--}

main :: IO ()
main = do
    [nombre] <- getArgs
    source <- readFile nombre
    let lista = alexScanTokens source
    if hay_error lista 
        then mapM_ (putStrLn . show) (filter es_error lista)
        else mapM_ (putStrLn . show) lista

