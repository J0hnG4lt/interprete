{
module Main where
import LexBot --(alexScanTokens, AlexPosn)
import System.Environment
}

-- Georvic Tur
-- 12-11402

%name calc            -- [ Token ] -> T
%tokentype { Token }
%error { parseError } -- Implementar Funcion parseError

%left "/\\" "\\/"
%nonassoc '=' "<=" ">=" "/=" '<' '>'
%left '*' '/'
%left '%'
%left NEG
    -- Definicion de Token

%token
    "create"      {TkCreate (AlexPn _ _ _)}        --
    "execute"     {TkExecute (AlexPn _ _ _)}       --
    "end"         {TkEnd (AlexPn _ _ _)}           --
    "bot"         {TkBot (AlexPn _ _ _)}           --
    "int"         {TkInt (AlexPn _ _ _)}           --
    "bool"        {TkBool (AlexPn _ _ _)}          --
    "char"        {TkChar (AlexPn _ _ _)}          --
    "store"       {TkStore (AlexPn _ _ _)}         --
    "receive"     {TkReceive (AlexPn _ _ _)}
    "on"          {TkOn (AlexPn _ _ _)}            --
    "activate"    {TkActivate (AlexPn _ _ _)}      --
    "advance"     {TkAdvance (AlexPn _ _ _)}       --
    "deactivate"  {TkDeactivate (AlexPn _ _ _)}    --
    "if"          {TkIf (AlexPn _ _ _)}            --
    "else"        {TkElse (AlexPn _ _ _)}          --
    "while"       {TkWhile (AlexPn _ _ _)}         --
    "collect"     {TkCollect (AlexPn _ _ _)}       --
    "as"          {TkAs (AlexPn _ _ _)}            --
    "drop"        {TkDrop (AlexPn _ _ _)}          --
    "up"          {TkUp (AlexPn _ _ _)}            --
    "down"        {TkDown (AlexPn _ _ _)}          --
    "left"        {TkLeft (AlexPn _ _ _)}          --
    "right"       {TkRight (AlexPn _ _ _)}         --
    "read"        {TkRead (AlexPn _ _ _)}          --
    "send"        {TkSend (AlexPn _ _ _)}          --
    "activation"  {TkActivation (AlexPn _ _ _)}            --
    "deactivation"    {TkDeactivation (AlexPn _ _ _)}      --
    "default"         {TkDefault (AlexPn _ _ _)}           --
    "me"              {TkMe (AlexPn _ _ _)}
    '+'             {TkSuma (AlexPn _ _ _)}                --
    '-'             {TkResta (AlexPn _ _ _)}               --
    '*'             {TkMult (AlexPn _ _ _)}                --
    '/'             {TkDiv (AlexPn _ _ _)}                 --
    '%'             {TkMod (AlexPn _ _ _)}                 --
    "/\\"            {TkConjuncion (AlexPn _ _ _)}         --
    "\/"            {TkDisyuncion (AlexPn _ _ _)}          --
    '~'             {TkNegacion (AlexPn _ _ _)}            --
    '<'             {TkMenor (AlexPn _ _ _)}               --
    "<="            {TkMenorIgual (AlexPn _ _ _)}          --
    '>'             {TkMayor (AlexPn _ _ _)}               --
    ">="            {TkMayorIgual (AlexPn _ _ _)}          --
    '='             {TkIgual (AlexPn _ _ _)}               --
    "/="            {TkDesigualdad (AlexPn _ _ _)}         --
    Variable             {TkIdent (AlexPn _ _ _) $$}       --
    Constante           {TkNum (AlexPn _ _ _) $$}          --
    "true"            {TkTrue (AlexPn _ _ _)}              --
    "false"           {TkFalse (AlexPn _ _ _)}             --
    Letra           {TkCaracter (AlexPn _ _ _) $$}
    ','             {TkComa (AlexPn _ _ _)}                --
    '.'             {TkPunto (AlexPn _ _ _)}               --
    ':'             {TkDosPuntos (AlexPn _ _ _)}           --
    '('             {TkParAbre (AlexPn _ _ _)}             --
    ')'             {TkParCierra (AlexPn _ _ _)}           --


%%

Secuencia :: {AST}
Secuencia : "create" Lista_de_Declaraciones "execute" Instrucciones "end"      {Sec_Dec $2 $4}
    |   "execute" Instrucciones "end"                                          {Sec $2}

Definicion_de_Robot :: {DefRob}
Definicion_de_Robot : Tipo "bot" Lista_de_Identificadores Lista_de_Comportamientos "end"    {DefRob_Full $1 $3 $4}
    |   Tipo "bot" Lista_de_Identificadores "end"                                           {DefRob_Empty $1 $3}

Lista_de_Declaraciones :: {ListDecl}
Lista_de_Declaraciones : Lista_de_Declaraciones Definicion_de_Robot     {ListDecl_L ( $2 : (getDecl_L $1))}
    |   Definicion_de_Robot                                             {ListDecl_L [$1] }

Tipo :: {Tipo}
Tipo : "int"      {TInt}
    | "bool"      {TBool}
    | "char"      {TChar}

--Int : {Int}
--Bool : {Bool}
--Char : {Char}

Lista_de_Comportamientos :: {ListComp}
Lista_de_Comportamientos : Lista_de_Comportamientos Comportamiento      {ListComp_L ($2 : (getComp_L $1))}
    |   Comportamiento                                                  {ListComp_L [$1] }

Comportamiento :: {Comp}
Comportamiento : "on" Condicion ':' Instruccion_de_Robot "end"          {Comp $2 $4}

Lista_de_Identificadores :: {ListIdent}
Lista_de_Identificadores : Lista_de_Identificadores ',' Variable        {ListIdent_V ((Var_C $3) : (getIdent_V $1)) }
    |   Variable                                                        {ListIdent_V [Var_C $1] }


Condicion :: {Cond}
Condicion : "activation"      {Activation}
    |   "deactivation"        {Deactivation}
    |   "default"             {Default}

--Activation : {Activation}
--Deactivation : {Deactivation}
--Default : {Default}

Instruccion_de_Controlador :: {InstContr}
Instruccion_de_Controlador : "activate" Lista_de_Identificadores        {ActivateInst $2}
    |   "advance" Lista_de_Identificadores                              {AdvanceInst $2}
    |   "deactivate" Lista_de_Identificadores                           {DeactivateInst $2}

Instrucciones :: {Instrcs}
Instrucciones : Secuenciacion                   {Instrcs_S $1}
    |       Iteracion_Indeterminada             {Instrcs_W $1}
    |       Condicional                         {Instrcs_I $1}

Secuenciacion :: {Secuen}
Secuenciacion : Secuenciacion Instruccion_de_Controlador        {Secuen ($2 : (getInstContr $1))}
    |   Instruccion_de_Controlador                              {Secuen [$1]}

Condicional :: {IfCond}
Condicional : "if" Expresion_Bool Secuenciacion "else" Secuenciacion "end"        {IfCond_Else $2 $3 $5}
    |   "if" Expresion_Bool Secuenciacion "end"                                   {IfCond_Pass $2 $3}

Iteracion_Indeterminada :: {While}
Iteracion_Indeterminada : "while" Expresion_Bool ':' Secuenciacion "end"          {While $2 $4}

{-
Instruccion_de_Robot : Almacenado '.'       {InstRob $1}
    |   Coleccion '.'                       {InstRob $1}
    |   Soltado '.'                         {InstRob $1}
    |   Movimiento '.'                      {InstRob $1}
    |   Entrada_y_Salida '.'                {InstRob $1}
--    |   Secuenciacion_Inst '.'              {InstRob $1}
-}

{-
Coleccion : "collect" "as" Identificador      {Colec $3}
    | "collect"                               {Colec_empty}

Identificador : Variable                      {Var $1}

Soltado : "drop" Expresion                      {Solt $2}

Movimiento : Direccion Expresion                {Mov $1 $2}
    | Direccion                                 {Mov_empty $1}

Almacenado : "store" Expresion                  {Almac $2}

Entrada_y_Salida :: {InstRob}
Entrada_y_Salida : "read" "as" Variable        {ES_Read $3}
    |   "read"                                      {ES_Empty_Read}
    |   "send"                                      {ES_Empty_Send}

-}

Instruccion_de_Robot :: {InstRob}
Instruccion_de_Robot : "store" Expresion '.' {Almac $2}
    |   "collect" "as" Variable '.'        {Colec (Var_C $3)}
    |   "collect" '.'                           {Colec_empty}
    |   "drop" Expresion '.'                    {Solt $2}
    |   Direccion Expresion '.'                 {Mov $1 $2}
    |   Direccion '.'                           {Mov_empty $1}
    |   "read" "as" Variable '.'                {ES_Read (Var_C $3)}
    |   "read"                                      {ES_Empty_Read}
    |   "send"                                      {ES_Empty_Send}

Direccion :: {Dir}
Direccion : "left"        {DLeft}
    |   "right"           {DRight}
    |   "up"              {DUp}
    |   "down"            {DDown}

--Left : {Left}
--Up : {Up}
--Right : {Right}
--Down : {Down}

Expresion :: {Expr}
Expresion : Expresion_Bool                          {Expr_Bool_ $1}
    |   Expresion_Num                               {Expr_Num_ $1}

Expresion_Num :: {Expr_Num}
Expresion_Num : Expresion_Num '+' Expresion_Num     {Suma $1 $3}
    |   Expresion_Num '*' Expresion_Num             {Produ $1 $3}
    |   Expresion_Num '/' Expresion_Num             {Divi $1 $3}
    |   Expresion_Num '%' Expresion_Num             {Modu $1 $3}
    |   Expresion_Num '-' Expresion_Num             {Resta $1 $3}
    |   '-' Expresion_Num    %prec NEG              {Nega $2}
    |   '(' Expresion_Num ')'                            {Parentesis_Num $2}
    |   Variable                                    {Variabl_N (Var_C $1)}
    |   Constante                                   {Numer $1}

Expresion_Bool :: {Expr_Bool}
Expresion_Bool : Expresion_Bool "/\\" Expresion_Bool    {And_ $1 $3}
    |   Expresion_Bool "\/" Expresion_Bool             {Or_ $1 $3}
    |   '~' Expresion_Bool                              {Not_ $2}
    |   Expresion_Bool '=' Expresion_Bool               {EquBool $1 $3}
    |   Expresion_Bool "/=" Expresion_Bool              {NotEquBool $1 $3}
    |   Expresion_Num "<=" Expresion_Num                {MenorEqu $1 $3}
    |   Expresion_Num '<' Expresion_Num                 {Menor $1 $3}
    |   Expresion_Num ">=" Expresion_Num                {MayorEqu $1 $3}
    |   Expresion_Num '>' Expresion_Num                 {Mayor $1 $3}
    |   Expresion_Num '=' Expresion_Num                 {EquNum $1 $3}
    |   Expresion_Num "/=" Expresion_Num                {NotEquNum $1 $3}
    |   '(' Expresion_Bool ')'                               {Parentesis_Bool $2}
    |   Variable                                        {Variabl_B (Var_C $1)}
    |   "true"                                          {Booleano True}
    |   "false"                                         {Booleano False}



{

data AST = Sec Instrcs
        | Sec_Dec ListDecl Instrcs
        deriving (Eq, Show)

data Instrcs = Instrcs_S Secuen
        | Instrcs_W While
        | Instrcs_I IfCond
        deriving (Eq, Show)

data While = While Expr_Bool Secuen
        deriving (Eq, Show)

data IfCond = IfCond_Else Expr_Bool Secuen Secuen
        |   IfCond_Pass Expr_Bool Secuen
        deriving (Eq, Show)

data Dir = DLeft | DRight | DUp | DDown
        deriving (Eq, Show)

data Secuen = Secuen { getInstContr :: [InstContr]}
        deriving (Eq, Show)

data ListDecl = ListDecl_L { getDecl_L :: [DefRob]}
        deriving (Eq, Show)

data DefRob = DefRob_Full Tipo ListIdent ListComp
        |   DefRob_Empty Tipo ListIdent
        deriving (Eq, Show)

data Tipo = TInt | TBool | TChar
        deriving (Eq, Show)

data ListIdent = ListIdent_V { getIdent_V :: [Var]}
        deriving (Eq, Show)

data ListComp = ListComp_L {getComp_L :: [Comp]}
        deriving (Eq, Show)

data Comp = Comp Cond InstRob
        deriving (Eq, Show)

data Cond = Activation 
        | Deactivation 
        | Default 
        | Expresion
        deriving (Eq, Show)

data InstRob = Almac Expr
            |   Colec Var
            |   Solt Expr
            |   Mov Dir Expr
            |   ES_Read Var
            |   ES_Empty_Read
            |   ES_Empty_Send
            |   Colec_empty
            |   Mov_empty Dir
        deriving (Eq, Show)
--            |   Sec_Inst -- Falta

data Var = Var_C String
        deriving (Eq, Show)

data InstContr = ActivateInst ListIdent
        |   DeactivateInst ListIdent
        |   AdvanceInst ListIdent
        deriving (Eq, Show)

data Expr = Expr_Bool_ Expr_Bool | Expr_Num_ Expr_Num
        deriving (Eq, Show)

data Expr_Bool = And_ Expr_Bool Expr_Bool
        |       Or_ Expr_Bool Expr_Bool
        |       Not_ Expr_Bool
        |       EquBool Expr_Bool Expr_Bool
        |       NotEquBool Expr_Bool Expr_Bool
        |       MenorEqu Expr_Num Expr_Num
        |       Menor Expr_Num Expr_Num
        |       MayorEqu Expr_Num Expr_Num
        |       Mayor Expr_Num Expr_Num
        |       EquNum Expr_Num Expr_Num
        |       NotEquNum Expr_Num Expr_Num
        |       Variabl_B Var
        |       Booleano Bool
        |       Parentesis_Bool Expr_Bool
        deriving (Eq, Show)

data Expr_Num = Suma Expr_Num Expr_Num
        |       Resta Expr_Num Expr_Num
        |       Divi Expr_Num Expr_Num
        |       Produ Expr_Num Expr_Num
        |       Modu Expr_Num Expr_Num
        |       Nega Expr_Num
        |       Variabl_N Var
        |       Numer Int
        |       Parentesis_Num Expr_Num
        deriving (Eq, Show)
{-
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
        TkError    AlexPosn   Char  -- Se definio el token de error
        deriving (Eq, Show)
-}
    -- Funcion de Error
parseError :: [Token] -> a
parseError _ = error "Error de Parser"

--main = getContents >>= print . calc . alexScanTokens


main :: IO ()
main = do
    [nombre] <- getArgs
    source <- readFile nombre
    let lista = alexScanTokens source
    let arbol_sintactico = calc lista
    print arbol_sintactico
}



