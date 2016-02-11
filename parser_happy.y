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
    CREATE      {TkCreate (AlexPn _ _ _)}        --
    EXECUTE     {TkExecute (AlexPn _ _ _)}       --
    END         {TkEnd (AlexPn _ _ _)}           --
    BOT         {TkBot (AlexPn _ _ _)}           --
    INT         {TkInt (AlexPn _ _ _)}           --
    BOOL        {TkBool (AlexPn _ _ _)}          --
    CHAR        {TkChar (AlexPn _ _ _)}          --
    STORE       {TkStore (AlexPn _ _ _)}         --
    RECEIVE     {TkReceive (AlexPn _ _ _)}
    ON          {TkOn (AlexPn _ _ _)}            --
    ACTIVATE    {TkActivate (AlexPn _ _ _)}      --
    ADVANCE     {TkAdvance (AlexPn _ _ _)}       --
    DEACTIVATE  {TkDeactivate (AlexPn _ _ _)}    --
    IF          {TkIf (AlexPn _ _ _)}            --
    ELSE        {TkElse (AlexPn _ _ _)}          --
    WHILE       {TkWhile (AlexPn _ _ _)}         --
    COLLECT     {TkCollect (AlexPn _ _ _)}       --
    AS          {TkAs (AlexPn _ _ _)}            --
    DROP        {TkDrop (AlexPn _ _ _)}          --
    UP          {TkUp (AlexPn _ _ _)}            --
    DOWN        {TkDown (AlexPn _ _ _)}          --
    LEFT        {TkLeft (AlexPn _ _ _)}          --
    RIGHT       {TkRight (AlexPn _ _ _)}         --
    READ        {TkRead (AlexPn _ _ _)}          --
    SEND        {TkSend (AlexPn _ _ _)}          --
    ACTIVATION  {TkActivation (AlexPn _ _ _)}            --
    DEACTIVATION    {TkDeactivation (AlexPn _ _ _)}      --
    DEFAULT         {TkDefault (AlexPn _ _ _)}           --
    ME              {TkMe (AlexPn _ _ _)}
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
    TRUE            {TkTrue (AlexPn _ _ _)}              --
    FALSE           {TkFalse (AlexPn _ _ _)}             --
    Letra           {TkCaracter (AlexPn _ _ _) $$}
    ','             {TkComa (AlexPn _ _ _)}                --
    '.'             {TkPunto (AlexPn _ _ _)}               --
    ':'             {TkDosPuntos (AlexPn _ _ _)}           --
    '('             {TkParAbre (AlexPn _ _ _)}             --
    ')'             {TkParCierra (AlexPn _ _ _)}           --


%%

Secuencia :: {AST}
Secuencia : CREATE Lista_de_Declaraciones EXECUTE Instrucciones END      {Sec_Dec $2 $4}
    |   EXECUTE Instrucciones END                                          {Sec $2}

Definicion_de_Robot :: {DefRob}
Definicion_de_Robot : Tipo BOT Lista_de_Identificadores Lista_de_Comportamientos END    {DefRob_Full $1 $3 $4}
    |   Tipo BOT Lista_de_Identificadores END                                           {DefRob_Empty $1 $3}

Lista_de_Declaraciones :: {ListDecl}
Lista_de_Declaraciones : Lista_de_Declaraciones Definicion_de_Robot     {ListDecl_L ( $2 : (getDecl_L $1))}
    |   Definicion_de_Robot                                             {ListDecl_L [$1] }

Tipo :: {Tipo}
Tipo : INT      {TInt}
    | BOOL      {TBool}
    | CHAR      {TChar}

--Int : {Int}
--Bool : {Bool}
--Char : {Char}

Lista_de_Comportamientos :: {ListComp}
Lista_de_Comportamientos : Lista_de_Comportamientos Comportamiento      {ListComp_L ($2 : (getComp_L $1))}
    |   Comportamiento                                                  {ListComp_L [$1] }

Comportamiento :: {Comp}
Comportamiento : ON Condicion ':' Instruccion_de_Robot END          {Comp $2 $4}

Lista_de_Identificadores :: {ListIdent}
Lista_de_Identificadores : Lista_de_Identificadores ',' Variable        {ListIdent_V ((Var_C $3) : (getIdent_V $1)) }
    |   Variable                                                        {ListIdent_V [Var_C $1] }


Condicion :: {Cond}
Condicion : ACTIVATION      {Activation}
    |   DEACTIVATION        {Deactivation}
    |   DEFAULT             {Default}
    |   Expresion           {Cond_Expr $1}

--Activation : {Activation}
--Deactivation : {Deactivation}
--Default : {Default}

Instruccion_de_Controlador :: {InstContr}
Instruccion_de_Controlador : ACTIVATE Lista_de_Identificadores '.'       {ActivateInst $2}
    |   ADVANCE Lista_de_Identificadores '.'                             {AdvanceInst $2}
    |   DEACTIVATE Lista_de_Identificadores  '.'                         {DeactivateInst $2}

Instrucciones :: {Instrcs}
Instrucciones : Instrucciones Instrucciones     {Instrcs_Varios $1 $2}
    |       Secuenciacion                       {Instrcs_S $1}
    |       Iteracion_Indeterminada             {Instrcs_W $1}
    |       Condicional                         {Instrcs_I $1}

Secuenciacion :: {Secuen}
Secuenciacion : Secuenciacion Instruccion_de_Controlador        {Secuen ($2 : (getInstContr $1))}
    |   Instruccion_de_Controlador                              {Secuen [$1]}

Condicional :: {IfCond}
Condicional : IF Expresion ':' Instrucciones ELSE Instrucciones END        {IfCond_Else $2 $4 $6}
    |   IF Expresion ':' Instrucciones END                                   {IfCond_Pass $2 $4}

Iteracion_Indeterminada :: {While}
Iteracion_Indeterminada : WHILE Expresion ':' Instrucciones END          {While $2 $4}

{-
Instruccion_de_Robot : Almacenado '.'       {InstRob $1}
    |   Coleccion '.'                       {InstRob $1}
    |   Soltado '.'                         {InstRob $1}
    |   Movimiento '.'                      {InstRob $1}
    |   Entrada_y_Salida '.'                {InstRob $1}
--    |   Secuenciacion_Inst '.'              {InstRob $1}
-}

{-
Coleccion : COLLECT AS Identificador      {Colec $3}
    | COLLECT                               {Colec_empty}

Identificador : Variable                      {Var $1}

Soltado : DROP Expresion                      {Solt $2}

Movimiento : Direccion Expresion                {Mov $1 $2}
    | Direccion                                 {Mov_empty $1}

Almacenado : STORE Expresion                  {Almac $2}

Entrada_y_Salida :: {InstRob}
Entrada_y_Salida : READ AS Variable        {ES_Read $3}
    |   READ                                      {ES_Empty_Read}
    |   SEND                                      {ES_Empty_Send}

-}

Instruccion_de_Robot :: {InstRob}
Instruccion_de_Robot : STORE Expresion '.' {Almac $2}
    |   COLLECT AS Expresion '.'        {Colec $3}
    |   COLLECT '.'                           {Colec_empty}
    |   DROP Expresion '.'                    {Solt $2}
    |   Direccion Expresion '.'                 {Mov $1 $2}
    |   Direccion '.'                           {Mov_empty $1}
    |   READ AS Variable '.'                {ES_Read (Var_C $3)}
    |   READ                                      {ES_Empty_Read}
    |   SEND                                      {ES_Empty_Send}
    |   RECEIVE '.'                                  {Receive}

Direccion :: {Dir}
Direccion : LEFT        {DLeft}
    |   RIGHT           {DRight}
    |   UP              {DUp}
    |   DOWN            {DDown}

--Left : {Left}
--Up : {Up}
--Right : {Right}
--Down : {Down}

Expresion :: {Expr}
Expresion :   Letra                                    {Expr_Char_ (CChar $1)}
    |   ME                                             {Expr_Me_ Me}
    |   Expresion '+' Expresion                        {Suma $1 $3}
    |   Expresion '*' Expresion                        {Produ $1 $3}
    |   Expresion '/' Expresion                        {Divi $1 $3}
    |   Expresion '%' Expresion                        {Modu $1 $3}
    |   Expresion '-' Expresion                        {Resta $1 $3}
    |   '-' Expresion    %prec NEG                     {Nega $2}
    |   '(' Expresion ')'                              {Parentesis $2}
    |   Variable                                       {Variabl (Var_C $1)}
    |   Constante                                      {Numer $1}
    |   Expresion "/\\" Expresion                      {And_ $1 $3}
    |   Expresion "\/" Expresion                       {Or_ $1 $3}
    |   '~' Expresion                                  {Not_ $2}
    |   Expresion '=' Expresion                        {Equ $1 $3}
    |   Expresion "/=" Expresion                       {NotEqu $1 $3}
    |   Expresion "<=" Expresion                       {MenorEqu $1 $3}
    |   Expresion '<' Expresion                        {Menor $1 $3}
    |   Expresion ">=" Expresion                       {MayorEqu $1 $3}
    |   Expresion '>' Expresion                        {Mayor $1 $3}
    |   TRUE                                           {Booleano True}
    |   FALSE                                          {Booleano False}



{

data AST = Sec Instrcs
        | Sec_Dec ListDecl Instrcs
        deriving (Eq)

instance Show AST where
    show (Sec instrcs) = "(Instrucciones "++(show instrcs)++")"
    show (Sec_Dec listdecl instrcs) = "(Declaraciones e Instrucciones "++(show listdecl)++" "++(show instrcs)++")"


data Instrcs = Instrcs_S Secuen
        | Instrcs_W While
        | Instrcs_I IfCond
        | Instrcs_Varios Instrcs Instrcs
        deriving (Eq)

instance Show Instrcs where
    show (Instrcs_S sec) = "(Instruccion Secuencia "++(show sec)++")"
    show (Instrcs_W whil) = "(Instruccion While "++(show whil)++")"
    show (Instrcs_I ifcon) = "(Instruccion If "++(show ifcon)++")"
    show (Instrcs_Varios inst1 inst2) = "(Varias Instrucciones"++(show inst1)++" "++(show inst2)++")"


data While = While Expr Instrcs
        deriving (Eq)

instance Show While where
    show (While expr sec) = "(While "++(show expr)++" "++(show sec)++")" 


data IfCond = IfCond_Else Expr Instrcs Instrcs
        |   IfCond_Pass Expr Instrcs
        deriving (Eq)

instance Show IfCond where
    show (IfCond_Else expr sec1 sec2) = "(If "++(show expr)++" "++(show sec1)++" "++(show sec2)++")"
    show (IfCond_Pass expr sec1) = "(If "++(show expr)++" "++(show sec1)++")"



data Dir = DLeft | DRight | DUp | DDown
        deriving (Eq)

instance Show Dir where
    show DLeft = "Izquierda"
    show DRight = "Derecha"
    show DUp = "Arriba"
    show DDown = "Abajo"

data Secuen = Secuen { getInstContr :: [InstContr]}
        deriving (Eq)

instance Show Secuen where
    show (Secuen instrcontrs) = "(Secuencia "++(show instrcontrs)++")"


data ListDecl = ListDecl_L { getDecl_L :: [DefRob]}
        deriving (Eq)

instance Show ListDecl where
    show (ListDecl_L defrobs) = "(Lista de Declaraciones "++(show defrobs)++")"


data DefRob = DefRob_Full Tipo ListIdent ListComp
        |   DefRob_Empty Tipo ListIdent
        deriving (Eq)

instance Show DefRob where
    show (DefRob_Full tipo listident listcomp) = "(Definicion Robot "++(show tipo)++" "++(show listident)++" "++(show listcomp)++")"


data Tipo = TInt | TBool | TChar
        deriving (Eq)

instance Show Tipo where
    show TInt = "Tipo Int"
    show TBool = "Tipo Bool"
    show TChar = "Tipo Char"


data ListIdent = ListIdent_V { getIdent_V :: [Var]}
        deriving (Eq)

instance Show ListIdent where
    show (ListIdent_V vars) = "(Lista Identificadores "++(show vars)++")"



data ListComp = ListComp_L {getComp_L :: [Comp]}
        deriving (Eq)

instance Show ListComp where
    show (ListComp_L comps) = "(Lista de Comportamientos "++(show comps)++")"     ------


data Comp = Comp Cond InstRob
        deriving (Eq)

instance Show Comp where
    show (Comp cond instrob) = "(Comportamiento on"++(show cond)++" "++(show instrob)++")"


data Cond = Activation 
        | Deactivation 
        | Default 
        | Cond_Expr Expr
        deriving (Eq)
     --   | Expresion

instance Show Cond where
    show Activation = "Activacion"
    show Deactivation = "Desactivacion"
    show Default = "Default"


data InstRob = Almac Expr
            |   Colec Expr
            |   Solt Expr
            |   Mov Dir Expr
            |   ES_Read Var
            |   ES_Empty_Read
            |   ES_Empty_Send
            |   Colec_empty
            |   Mov_empty Dir
            |   Receive
        deriving (Eq)
--            |   Sec_Inst -- Falta

instance Show InstRob where
    show (Almac expr) = "(Almacenar "++(show expr)++")"
    show (Colec expr) = "(Colectar "++(show expr)++")"
    show (Solt expr) = "(Soltar "++(show expr)++")"
    show (Mov dir expr) = "(Mover a la "++(show dir)++" "++(show expr)++")"
    show (ES_Read var) = "(Leer "++(show var)++")"
    show ES_Empty_Read = "(Leer)"
    show ES_Empty_Send = "(Enviar)"
    show (Mov_empty dir) = "(Mover a la"++(show dir)++")"
    show Receive = "(Recibir)"


data Var = Var_C String
        deriving (Eq)

instance Show Var where
    show (Var_C str) = str

data InstContr = ActivateInst ListIdent
        |   DeactivateInst ListIdent
        |   AdvanceInst ListIdent
        deriving (Eq)

instance Show InstContr where 
    show (ActivateInst list_ident) = "(Instruccion Activar "++(show list_ident)++")"
    show (DeactivateInst list_ident) = "(Instruccion Desactivar "++(show list_ident)++")"
    show (AdvanceInst list_ident) = "(Instruccion Avanzar "++(show list_ident)++")"


data Expr =     Expr_Char_ Expr_Char 
        |       Expr_Me_ Me
        |       Equ Expr Expr
        |       NotEqu Expr Expr
        |       And_ Expr Expr
        |       Or_ Expr Expr
        |       Not_ Expr
        |       MenorEqu Expr Expr
        |       Menor Expr Expr
        |       MayorEqu Expr Expr
        |       Mayor Expr Expr
        |       Variabl Var
        |       Booleano Bool
        |       Parentesis Expr
        |       Suma Expr Expr
        |       Resta Expr Expr
        |       Divi Expr Expr
        |       Produ Expr Expr
        |       Modu Expr Expr
        |       Nega Expr
        |       Numer Int
        deriving (Eq)

instance Show Expr where
    show (Expr_Char_ expr) = "(Caracter "++(show expr)++")"
    show (Equ expr1 expr2) = "(Igual "++(show expr1)++" "++(show expr2)++")"
    show (NotEqu expr1 expr2) = "(No Igual "++(show expr1)++" "++(show expr2)++")"
    show (And_ expr1 expr2) = "(And "++(show expr1)++" "++(show expr2)++")"
    show (Or_ expr1 expr2) = "(Or "++(show expr1)++" "++(show expr2)++")"
    show (Not_ expr1) = "(Not "++(show expr1)++")"
    show (MenorEqu expr1 expr2) = "(Menor Igual "++(show expr1)++" "++(show expr2)++")"
    show (Menor expr1 expr2) = "(Menor "++(show expr1)++" "++(show expr2)++")"
    show (MayorEqu expr1 expr2) = "(Mayor Igual "++(show expr1)++" "++(show expr2)++")"
    show (Mayor expr1 expr2) = "(Mayor "++(show expr1)++" "++(show expr2)++")"
    show (Variabl var) = "(Variable "++(show var)++")"
    show (Booleano bool) = "(Booleano "++(show bool)++")"
    show (Parentesis expr) = show expr
    show (Suma expr1 expr2) = "(Suma "++(show expr1)++" "++(show expr2)++")"
    show (Resta expr1 expr2) = "(Resta "++(show expr1)++" "++(show expr2)++")"
    show (Divi expr1 expr2) = "(Division "++(show expr1)++" "++(show expr2)++")"
    show (Produ expr1 expr2) = "(Producto "++(show expr1)++" "++(show expr2)++")"
    show (Modu expr1 expr2) = "(Modulo "++(show expr1)++" "++(show expr2)++")"
    show (Nega expr1) = "(Nega"++(show expr1)++")"
    show (Numer num) = "(Numero "++(show num)++")"
    show (Expr_Me_ me) = "me"



data Expr_Char = CChar Char
    deriving (Eq)

instance Show Expr_Char where
    show (CChar ch) = [ch]

data Me = Me
    deriving (Eq, Show)



    -- Funcion de Error
parseError :: [Token] -> a
parseError tokens = error ("\nPatron: Token[valor_de_token] numero_linea numero_columna\nToken inesperado a partir de \n" ++ (unwords (map show tokens)))

-- (_ ( AlexPn _ _ _))

--main = getContents >>= print . calc . alexScanTokens


main :: IO ()
main = do
    [nombre] <- getArgs
    source <- readFile nombre
    let lista = alexScanTokens source
    --(putStrLn . show) lista
    --putStrLn "Lista de Tokens obtenida..."
    let arbol_sintactico = calc lista
    (putStrLn . show) arbol_sintactico

}




