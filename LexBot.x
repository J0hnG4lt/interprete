{
module LexBot (alexScanTokens, Token(TkError)) where
}

        --  Autor: Georvic Tur

%wrapper "posn"

        -- MACROS



        -- TOKENS

tokens :-

    $white+                              ;  --Espacio en blanco
    \$\-([^\-]*|[\n\t]|\-+([^\-\$]|[\n\t]))*\-+\$    ;  --Comentarios de Bloque
    \$\$.*                             ;  --Comentarios de una linea
    create                               { \p s -> TkCreate p }
    execute                         { \p s -> TkExecute p }
    end                         { \p s -> TkEnd p }
    bot                         { \p s -> TkBot p }
    int                         { \p s -> TkInt p }
    bool                        { \p s -> TkBool p }
    char                        { \p s -> TkChar p }
    store                       { \p s -> TkStore p }
    receive                     { \p s -> TkReceive p }
    on                          { \p s -> TkOn p }
    activate                    { \p s -> TkActivate p }
    advance                     { \p s -> TkAdvance p }
    deactivate                  { \p s -> TkDeactivate p }
    if                          { \p s -> TkIf p }
    else                        { \p s -> TkElse p }
    while                       { \p s -> TkWhile p }
    collect                     { \p s -> TkCollect p }
    as                          { \p s -> TkAs p }
    drop                        { \p s -> TkDrop p }
    up                          { \p s -> TkUp p }
    down                        { \p s -> TkDown p }
    left                        { \p s -> TkLeft p }
    right                       { \p s -> TkRight p }
    read                        { \p s -> TkRead p }
    send                        { \p s -> TkSend p }
    activation                  { \p s -> TkActivation p }
    deactivation                { \p s -> TkDeactivation p }
    default                     { \p s -> TkDefault p }
    me                          { \p s -> TkMe p }
    true                        { \p s -> TkTrue p }
    false                       { \p s -> TkFalse p }
    \,                          { \p s -> TkComa p }
    \.                          { \p s -> TkPunto p }
    \:                          { \p s -> TkDosPuntos p }
    \(                          { \p s -> TkParAbre p }
    \)                          { \p s -> TkParCierra p }
    \+                          { \p s -> TkSuma p }
    \-                          { \p s -> TkResta p }
    \*                          { \p s -> TkMult p }
    \/                          { \p s -> TkDiv p }
    \%                          { \p s -> TkMod p }
    \/\\                        { \p s -> TkConjuncion p }
    \\\/                        { \p s -> TkDisyuncion p }
    \~                          { \p s -> TkNegacion p }
    \<                          { \p s -> TkMenor p }
    \<\=                        { \p s -> TkMenorIgual p }
    \>                          { \p s -> TkMayor p }
    \>=                         { \p s -> TkMayorIgual p }
    \/\=                        { \p s -> TkDesigualdad p }
    \=                          { \p s -> TkIgual p }
    [1-9][0-9]*                 { \p s -> TkNum p (read s) }
    [a-zA-Z0-9']                { \p s -> TkCaracter p (head s) }
    [a-zA-Z][a-zA-Z0-9\_]*      { \p s -> TkIdent p s }
    .                           { \p s -> TkError p (head s) }  --Caracter Erroneo


        -- CODIGO

{
        -- DEFINICION DEL TIPO TOKEN

data Token =
        TkCreate          AlexPosn  |
        TkExecute         AlexPosn  |
        TkEnd             AlexPosn  |
        TkBot             AlexPosn  |
        TkInt             AlexPosn  |
        TkBool            AlexPosn  |
        TkChar            AlexPosn  |
        TkStore           AlexPosn  |
        TkReceive         AlexPosn  |
        TkOn              AlexPosn  |
        TkActivate        AlexPosn  |
        TkAdvance         AlexPosn  |
        TkDeactivate      AlexPosn  |
        TkIf              AlexPosn  |
        TkElse            AlexPosn  |
        TkWhile           AlexPosn  |
        TkCollect         AlexPosn  |
        TkAs              AlexPosn  |
        TkDrop            AlexPosn  |
        TkUp              AlexPosn  |
        TkDown            AlexPosn  |
        TkLeft            AlexPosn  |
        TkRight           AlexPosn  |
        TkRead            AlexPosn  |
        TkSend            AlexPosn  |
        TkActivation      AlexPosn  |
        TkDeactivation    AlexPosn  |
        TkDefault         AlexPosn  |
        TkMe              AlexPosn  |
        TkSuma            AlexPosn  |
        TkResta           AlexPosn  |
        TkMult            AlexPosn  |
        TkDiv             AlexPosn  |
        TkMod             AlexPosn  |
        TkConjuncion      AlexPosn  |
        TkDisyuncion      AlexPosn  |
        TkNegacion        AlexPosn  |
        TkMenor           AlexPosn  |
        TkMenorIgual      AlexPosn  |
        TkMayor           AlexPosn  |
        TkMayorIgual      AlexPosn  |
        TkIgual           AlexPosn  |
        TkDesigualdad     AlexPosn  |
        TkIdent   AlexPosn  String  |
        TkNum     AlexPosn     Int  |
        TkTrue            AlexPosn  |
        TkFalse           AlexPosn  |
        TkCaracter AlexPosn   Char  |
        TkComa            AlexPosn  |
        TkPunto           AlexPosn  |
        TkDosPuntos       AlexPosn  |
        TkParAbre         AlexPosn  |
        TkParCierra       AlexPosn  |
        TkError    AlexPosn   Char
        deriving (Eq)

instance Show Token where
    show cons = imprimir_token cons

imprimir_token :: Token -> String

imprimir_token (TkCreate (AlexPn _ linea columna)) = "TkCreate "++show(linea)++" "++show(columna)
imprimir_token (TkExecute (AlexPn _ linea columna)) = "TkExecute "++show(linea)++" "++show(columna)
imprimir_token (TkEnd (AlexPn _ linea columna)) = "TkEnd "++show(linea)++" "++show(columna)
imprimir_token (TkBot (AlexPn _ linea columna)) = "TkBot "++show(linea)++" "++show(columna)
imprimir_token (TkInt (AlexPn _ linea columna)) = "TkInt "++show(linea)++" "++show(columna)
imprimir_token (TkBool (AlexPn _ linea columna)) = "TkBool "++show(linea)++" "++show(columna)
imprimir_token (TkChar (AlexPn _ linea columna)) = "TkChar "++show(linea)++" "++show(columna)
imprimir_token (TkStore (AlexPn _ linea columna)) = "TkStore "++show(linea)++" "++show(columna)
imprimir_token (TkReceive (AlexPn _ linea columna)) = "TkReceive "++show(linea)++" "++show(columna)
imprimir_token (TkOn (AlexPn _ linea columna)) = "TkOn "++show(linea)++" "++show(columna)
imprimir_token (TkActivate (AlexPn _ linea columna)) = "TkActivate "++show(linea)++" "++show(columna)
imprimir_token (TkAdvance (AlexPn _ linea columna)) = "TkAdvance "++show(linea)++" "++show(columna)
imprimir_token (TkDeactivate (AlexPn _ linea columna)) = "TkDeactivate "++show(linea)++" "++show(columna)
imprimir_token (TkIf (AlexPn _ linea columna)) = "TkIf "++show(linea)++" "++show(columna)
imprimir_token (TkElse (AlexPn _ linea columna)) = "TkElse "++show(linea)++" "++show(columna)
imprimir_token (TkWhile (AlexPn _ linea columna)) = "TkWhile "++show(linea)++" "++show(columna)
imprimir_token (TkCollect (AlexPn _ linea columna)) = "TkCollect "++show(linea)++" "++show(columna)
imprimir_token (TkAs (AlexPn _ linea columna)) = "TkAs "++show(linea)++" "++show(columna)
imprimir_token (TkDrop (AlexPn _ linea columna)) = "TkDrop "++show(linea)++" "++show(columna)
imprimir_token (TkUp (AlexPn _ linea columna)) = "TkUp "++show(linea)++" "++show(columna)
imprimir_token (TkDown (AlexPn _ linea columna)) = "TkDown "++show(linea)++" "++show(columna)
imprimir_token (TkLeft (AlexPn _ linea columna)) = "TkLeft "++show(linea)++" "++show(columna)
imprimir_token (TkRight (AlexPn _ linea columna)) = "TkRight "++show(linea)++" "++show(columna)
imprimir_token (TkRead (AlexPn _ linea columna)) = "TkRead "++show(linea)++" "++show(columna)
imprimir_token (TkSend (AlexPn _ linea columna)) = "TkSend "++show(linea)++" "++show(columna)
imprimir_token (TkActivation (AlexPn _ linea columna)) = "TkActivation "++show(linea)++" "++show(columna)
imprimir_token (TkDeactivation (AlexPn _ linea columna)) = "TkDeactivation "++show(linea)++" "++show(columna)
imprimir_token (TkDefault (AlexPn _ linea columna)) = "TkDefault "++show(linea)++" "++show(columna)
imprimir_token (TkMe (AlexPn _ linea columna)) = "TkMe "++show(linea)++" "++show(columna)
imprimir_token (TkSuma (AlexPn _ linea columna)) = "TkSuma "++show(linea)++" "++show(columna)
imprimir_token (TkResta (AlexPn _ linea columna)) = "TkResta "++show(linea)++" "++show(columna)
imprimir_token (TkMult (AlexPn _ linea columna)) = "TkMult "++show(linea)++" "++show(columna)
imprimir_token (TkDiv (AlexPn _ linea columna)) = "TkDiv "++show(linea)++" "++show(columna)
imprimir_token (TkMod (AlexPn _ linea columna)) = "TkMod "++show(linea)++" "++show(columna)
imprimir_token (TkConjuncion (AlexPn _ linea columna)) = "TkConjuncion "++show(linea)++" "++show(columna)
imprimir_token (TkDisyuncion (AlexPn _ linea columna)) = "TkDisyuncion "++show(linea)++" "++show(columna)
imprimir_token (TkNegacion (AlexPn _ linea columna)) = "TkNegacion "++show(linea)++" "++show(columna)
imprimir_token (TkMenor (AlexPn _ linea columna)) = "TkMenor "++show(linea)++" "++show(columna)
imprimir_token (TkMenorIgual (AlexPn _ linea columna)) = "TkMenorIgual "++show(linea)++" "++show(columna)
imprimir_token (TkMayor (AlexPn _ linea columna)) = "TkMayor "++show(linea)++" "++show(columna)
imprimir_token (TkMayorIgual (AlexPn _ linea columna)) = "TkMayorIgual "++show(linea)++" "++show(columna)
imprimir_token (TkIgual (AlexPn _ linea columna)) = "TkIgual "++show(linea)++" "++show(columna)
imprimir_token (TkDesigualdad (AlexPn _ linea columna)) = "TkDesigualdad "++show(linea)++" "++show(columna)
imprimir_token (TkTrue (AlexPn _ linea columna)) = "TkTrue "++show(linea)++" "++show(columna)
imprimir_token (TkFalse (AlexPn _ linea columna)) = "TkFalse "++show(linea)++" "++show(columna)
imprimir_token (TkComa (AlexPn _ linea columna)) = "TkComa "++show(linea)++" "++show(columna)
imprimir_token (TkPunto (AlexPn _ linea columna)) = "TkPunto "++show(linea)++" "++show(columna)
imprimir_token (TkDosPuntos (AlexPn _ linea columna)) = "TkDosPuntos "++show(linea)++" "++show(columna)
imprimir_token (TkParCierra (AlexPn _ linea columna)) = "TkParCierra "++show(linea)++" "++show(columna)
imprimir_token (TkParAbre (AlexPn _ linea columna)) = "TkParAbre "++show(linea)++" "++show(columna)

imprimir_token (TkError (AlexPn _ linea columna) elem) = "Error: Caracter inesperado "
                ++show(elem)++" en la fila "++show(linea)++", columna "++show(columna)

imprimir_token (TkCaracter (AlexPn _ linea columna) elem) = "TkCaracter("++show(elem)++") "++show(linea)++" "++show(columna)
imprimir_token (TkNum (AlexPn _ linea columna) elem) = "TkNum("++show(elem)++") "++show(linea)++" "++show(columna)
imprimir_token (TkIdent (AlexPn _ linea columna) elem) = "TkIdent("++show(elem)++") "++show(linea)++" "++show(columna)


{--

main :: IO ()
main = do
    source <- getContents
    let lista_tokens = alexScanTokens source
    mapM_ (putStrLn . show) lista_tokens
--}
}
