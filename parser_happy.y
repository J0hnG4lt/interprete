{
module Main where
import LexBot
import System.Environment
import Data.String
import Data.List


}

-- Georvic Tur
-- Carnet: 12-11402
-- Correo: alexanderstower@gmail.com

%name calc
%tokentype { Token }
%error { parseError } 


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
Secuencia : CREATE Lista_de_Declaraciones EXECUTE ListaInstrucciones END      {Sec_Dec $2 $4}
    |   EXECUTE ListaInstrucciones END                                          {Sec $2}


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


Lista_de_Comportamientos :: {ListComp}
Lista_de_Comportamientos : Lista_de_Comportamientos Comportamiento      {ListComp_L ($2 : (getComp_L $1))}
    |   Comportamiento                                                  {ListComp_L [$1] }

Comportamiento :: {Comp}
Comportamiento : ON Condicion ':' Lista_Instruccion_de_Robot END          {Comp $2 $4}

Lista_de_Identificadores :: {ListIdent}
Lista_de_Identificadores : Lista_de_Identificadores ',' Identificador_        {ListIdent_V ((Identific_ $3) : (getIdent_V $1)) }
    |   Identificador_                                                        {ListIdent_V [Identific_ $1] }

Identificador_ :: {Var}
Identificador_ : Variable           {Var_C $1}
    |   Letra                       {Var_C [$1]}


Condicion :: {Cond}
Condicion : ACTIVATION      {Activation}
    |   DEACTIVATION        {Deactivation}
    |   DEFAULT             {Default}
    |   Expresion           {Cond_Expr $1}


Instruccion_de_Controlador :: {InstContr}
Instruccion_de_Controlador : ACTIVATE Lista_de_Identificadores '.'       {ActivateInst $2}
    |   ADVANCE Lista_de_Identificadores '.'                             {AdvanceInst $2}
    |   DEACTIVATE Lista_de_Identificadores  '.'                         {DeactivateInst $2}

Instrucciones :: {Instrcs}
Instrucciones : Secuenciacion                       {Instrcs_S $1}
    |       Iteracion_Indeterminada             {Instrcs_W $1}
    |       Condicional                         {Instrcs_I $1}
    |       Secuencia                           {Instrcs_Alcance $1}


ListaInstrucciones :: {ListInstrcs}
ListaInstrucciones : ListaInstrucciones Instrucciones {ListInstrcs_L ($2 : (getInstrcs_L $1))}
    |       Instrucciones                             {ListInstrcs_L [$1] }

Secuenciacion :: {Secuen}
Secuenciacion : Secuenciacion Instruccion_de_Controlador        {Secuen ($2 : (getInstContr $1))}
    |   Instruccion_de_Controlador                              {Secuen [$1]}

Condicional :: {IfCond}
Condicional : IF Expresion ':' ListaInstrucciones ELSE ListaInstrucciones END        {IfCond_Else $2 $4 $6}
    |   IF Expresion ':' ListaInstrucciones END                                   {IfCond_Pass $2 $4}

Iteracion_Indeterminada :: {While}
Iteracion_Indeterminada : WHILE Expresion ':' ListaInstrucciones END          {While $2 $4}


Instruccion_de_Robot :: {InstRob}
Instruccion_de_Robot : STORE Expresion '.' {Almac $2}
    |   COLLECT AS Expresion '.'        {Colec $3}
    |   COLLECT '.'                           {Colec_empty}
    |   DROP Expresion '.'                    {Solt $2}
    |   Direccion Expresion '.'                 {Mov $1 $2}
    |   Direccion '.'                           {Mov_empty $1}
    |   READ AS Identificador_ '.'                {ES_Read (Identific_ $3)}
    |   READ  '.'                                   {ES_Empty_Read}
    |   SEND  '.'                                   {ES_Empty_Send}
    |   RECEIVE '.'                                  {Receive}


Lista_Instruccion_de_Robot :: {ListInstRob}
Lista_Instruccion_de_Robot : Lista_Instruccion_de_Robot Instruccion_de_Robot     {ListInstRob_L ($2 : (getInstRob_L $1))}
    |   Instruccion_de_Robot                                                     {ListInstRob_L [$1] }


Direccion :: {Dir}
Direccion : LEFT        {DLeft}
    |   RIGHT           {DRight}
    |   UP              {DUp}
    |   DOWN            {DDown}



Expresion :: {Expr}
Expresion : 
        ME                                             {Expr_Me_ Me}
    |   Expresion '+' Expresion                        {Suma $1 $3}
    |   Expresion '*' Expresion                        {Produ $1 $3}
    |   Expresion '/' Expresion                        {Divi $1 $3}
    |   Expresion '%' Expresion                        {Modu $1 $3}
    |   Expresion '-' Expresion                        {Resta $1 $3}
    |   '-' Expresion    %prec NEG                     {Nega $2}
    |   '(' Expresion ')'                              {Parentesis $2}
    |   Identificador_                                       {Variabl $1}
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

data AST = Sec ListInstrcs
        | Sec_Dec ListDecl ListInstrcs
        deriving (Eq)

instance Show AST where
    show (Sec instrcs) = "(Lista_Instrucciones "++(show instrcs)++")"
    show (Sec_Dec listdecl instrcs) = "(Declaraciones_e_Instrucciones "++(show listdecl)++" "++(show instrcs)++")"


data ListInstrcs = ListInstrcs_L {getInstrcs_L :: [Instrcs]}
    deriving (Eq)

instance Show ListInstrcs where
    show (ListInstrcs_L ins) = "(Lista_Instrucciones "++(show ins)++")"

data Instrcs = Instrcs_S Secuen
        | Instrcs_W While
        | Instrcs_I IfCond
        | Instrcs_Alcance AST
        deriving (Eq)

instance Show Instrcs where
    show (Instrcs_S sec) = "(Instruccion_Secuencia "++(show sec)++")"
    show (Instrcs_W whil) = "(Instruccion_While "++(show whil)++")"
    show (Instrcs_I ifcon) = "(Instruccion_If "++(show ifcon)++")"
    show (Instrcs_Alcance sec) = "(Alcance "++(show sec)++")"


data While = While Expr ListInstrcs
        deriving (Eq)

instance Show While where
    show (While expr sec) = "(While "++(show expr)++" "++(show sec)++")" 


data IfCond = IfCond_Else Expr ListInstrcs ListInstrcs
        |   IfCond_Pass Expr ListInstrcs
        deriving (Eq)

instance Show IfCond where
    show (IfCond_Else expr sec1 sec2) = "(If "++(show expr)++" "++(show sec1)++" "++(show sec2)++")"
    show (IfCond_Pass expr sec1) = "(If "++(show expr)++" "++(show sec1)++")"



data Dir = DLeft | DRight | DUp | DDown
        deriving (Eq)

instance Show Dir where
    show DLeft = "(Izquierda)"
    show DRight = "(Derecha)"
    show DUp = "(Arriba)"
    show DDown = "(Abajo)"

data Secuen = Secuen { getInstContr :: [InstContr]}
        deriving (Eq)

instance Show Secuen where
    show (Secuen instrcontrs) = "(Secuencia "++(show instrcontrs)++")"


data ListDecl = ListDecl_L { getDecl_L :: [DefRob]}
        deriving (Eq)

instance Show ListDecl where
    show (ListDecl_L defrobs) = "(Lista_de_Declaraciones "++(show defrobs)++")"


data DefRob = DefRob_Full Tipo ListIdent ListComp
        |   DefRob_Empty Tipo ListIdent
        deriving (Eq)

instance Show DefRob where
    show (DefRob_Full tipo listident listcomp) = "(Definicion_Robot "++(show tipo)++" "++(show listident)++" "++(show listcomp)++")"
    show (DefRob_Empty tip lista) = "(Definicion_Robot "++(show tip)++" "++(show lista)++")"


data Tipo = TInt | TBool | TChar
        deriving (Eq)

instance Show Tipo where
    show TInt = "(Tipo_Int)"
    show TBool = "(Tipo_Bool)"
    show TChar = "(Tipo_Char)"


data ListIdent = ListIdent_V { getIdent_V :: [Identific]}
        deriving (Eq)

instance Show ListIdent where
    show (ListIdent_V vars) = "(Lista_Identificadores "++(show vars)++")"

data Identific = Identific_ Var
    deriving (Eq)

instance Show Identific where
    show (Identific_ iden) = "(Identificador "++(show iden)++")" 

data Char_Expr = Char_Expr_ Char
    deriving(Eq, Show)

data ListComp = ListComp_L {getComp_L :: [Comp]}
        deriving (Eq)

instance Show ListComp where
    show (ListComp_L comps) = "(Lista_de_Comportamientos "++(show comps)++")"     ------


data Comp = Comp Cond ListInstRob
        deriving (Eq)

instance Show Comp where
    show (Comp cond instrob) = "(Comportamiento_on "++(show cond)++" "++(show instrob)++")"


data Cond = Activation 
        | Deactivation 
        | Default 
        | Cond_Expr Expr
        deriving (Eq)


instance Show Cond where
    show Activation = "(Activacion)"
    show Deactivation = "(Desactivacion)"
    show Default = "(Default)"
    show (Cond_Expr expr) = "(Expresion "++(show expr)++")"


data InstRob = Almac Expr
            |   Colec Expr
            |   Solt Expr
            |   Mov Dir Expr
            |   ES_Read Identific
            |   ES_Empty_Read
            |   ES_Empty_Send
            |   Colec_empty
            |   Mov_empty Dir
            |   Receive
        deriving (Eq)


instance Show InstRob where
    show (Almac expr) = "(Almacenar "++(show expr)++")"
    show (Colec expr) = "(Colectar "++(show expr)++")"
    show (Solt expr) = "(Soltar "++(show expr)++")"
    show (Mov dir expr) = "(Mover_a_la "++(show dir)++" "++(show expr)++")"
    show (ES_Read var) = "(Leer "++(show var)++")"
    show ES_Empty_Read = "(Leer)"
    show ES_Empty_Send = "(Enviar)"
    show (Mov_empty dir) = "(Mover_a_la "++(show dir)++")"
    show Receive = "(Recibir)"


data ListInstRob = ListInstRob_L {getInstRob_L :: [InstRob]}
    deriving (Eq)

instance Show ListInstRob where
    show (ListInstRob_L lista_inst_rob) = "(Lista_de_Instrucciones_Robot "++(show lista_inst_rob)++")"

data Var = Var_C [Char]
        deriving (Eq)

instance Show Var where
    show (Var_C str) = "("++str++")"

data InstContr = ActivateInst ListIdent
        |   DeactivateInst ListIdent
        |   AdvanceInst ListIdent
        deriving (Eq)

instance Show InstContr where 
    show (ActivateInst list_ident) = "(Instruccion_Activar "++(show list_ident)++")"
    show (DeactivateInst list_ident) = "(Instruccion_Desactivar "++(show list_ident)++")"
    show (AdvanceInst list_ident) = "(Instruccion_Avanzar "++(show list_ident)++")"


data Expr = 
               Expr_Me_ Me
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
    show (Equ expr1 expr2) = "(Igual "++(show expr1)++" "++(show expr2)++")"
    show (NotEqu expr1 expr2) = "(No_Igual "++(show expr1)++" "++(show expr2)++")"
    show (And_ expr1 expr2) = "(And "++(show expr1)++" "++(show expr2)++")"
    show (Or_ expr1 expr2) = "(Or "++(show expr1)++" "++(show expr2)++")"
    show (Not_ expr1) = "(Not "++(show expr1)++")"
    show (MenorEqu expr1 expr2) = "(Menor_Igual "++(show expr1)++" "++(show expr2)++")"
    show (Menor expr1 expr2) = "(Menor "++(show expr1)++" "++(show expr2)++")"
    show (MayorEqu expr1 expr2) = "(Mayor_Igual "++(show expr1)++" "++(show expr2)++")"
    show (Mayor expr1 expr2) = "(Mayor "++(show expr1)++" "++(show expr2)++")"
    show (Variabl var) = "(Identificador_ "++(show var)++")"
    show (Booleano bool) = "(Booleano "++(show bool)++")"
    show (Parentesis expr) = show expr
    show (Suma expr1 expr2) = "(Suma "++(show expr1)++" "++(show expr2)++")"
    show (Resta expr1 expr2) = "(Resta "++(show expr1)++" "++(show expr2)++")"
    show (Divi expr1 expr2) = "(Division "++(show expr1)++" "++(show expr2)++")"
    show (Produ expr1 expr2) = "(Producto "++(show expr1)++" "++(show expr2)++")"
    show (Modu expr1 expr2) = "(Modulo "++(show expr1)++" "++(show expr2)++")"
    show (Nega expr1) = "(Nega "++(show expr1)++")"
    show (Numer num) = "(Numero ("++(show num)++"))"
    show (Expr_Me_ me) = "(me)"


data Me = Me
    deriving (Eq, Show)



    -- Funcion de Error
parseError :: [Token] -> a
parseError tokens = error ("\nPatron: Token[valor_de_token] numero_linea numero_columna\nToken inesperado a partir de \n" ++ (unwords (map show tokens)))


type I_S = ([Int], String)

-- Asume que se quitan los parentesis exteriores de (parte1 (parte2) ... (parten))
-- Devuelve lista de indices donde hay espacios en el nivel de una misma regla
-- Estos indices permiten separar los componentes de una produccion
separar_componentes_i :: [Char] -> Int -> Int -> [Int]
separar_componentes_i arbol num_parentesis num_index = 
    case arbol of
        ('(':xs) -> separar_componentes_i xs (num_parentesis+1) (num_index +1)
        (')':xs) -> separar_componentes_i xs (num_parentesis-1) (num_index +1)
        (' ':xs) -> if num_parentesis == 0 
                        then num_index:(separar_componentes_i xs num_parentesis (num_index+1)) 
                        else separar_componentes_i xs num_parentesis (num_index+1)
        (_:xs) -> separar_componentes_i xs num_parentesis (num_index+1)
        [] -> []


resta :: Int -> Int -> Int
resta a b = b - a

-- Con los indices obtenidos puedo separar el primer componente
-- El resto se puede mantener como estado para volver a ser usado
pasar_de_estado :: I_S -> ( [String] , I_S )
pasar_de_estado (indices, arbol) = let (valor,resto) = head [(splitAt i arbol) | i <- indices ]
                                   in if not $ null indices 
                                        then ([valor], ((map (resta (length valor)) (tail indices)),resto))
                                      else
                                             ([valor], ([], resto))


obtener_valor :: ([String], I_S) -> [String]
obtener_valor (componentes, estado) = componentes


-- Pasa de un estado a otro extrayendo siempre los componentes en una lista
recur :: ([String], I_S) -> ([String], I_S)
recur (componentes, estado) =
    case estado of
        ([],resto) -> (componentes++[resto], estado)
        (_, _) -> let (val, est) = pasar_de_estado estado
                  in  recur (componentes++val, est)



separar_componentes :: [Char] -> [[Char]]
separar_componentes arbol = let indices = separar_componentes_i arbol 0 0
                            in  obtener_valor $ recur ([], (indices, arbol))



reemplazar :: Char -> Char -> Char -> Char
reemplazar ch1 ch2 ch3
   | ch1 == ch3 = ch2
   | otherwise = ch3

-- Permite identar
imprimir_espacios :: Int -> IO ()
imprimir_espacios num = putStr $ replicate num ' '


--Recibe el arbol sintactico tal como lo imprime su show
--En cada caso se quiere separar los componentes de alto nivel del arbol
-- Estos corresponden a una produccion y deben estar identados igual
-- Los subcomponentes deben identarse más
imprimir_arbol_parentisado :: Int -> String -> IO ()
imprimir_arbol_parentisado nivel arbol =
    do let inicial = head arbol
       case inicial of
            '(' -> do let sin_parentesis = (init . tail) arbol
                      let componentes = separar_componentes sin_parentesis
                      imprimir_espacios nivel
                      mapM_ (imprimir_arbol_parentisado (nivel+1)) componentes
            '[' -> do let sin_corchetes = (init . tail) arbol
                      let miembros = separar_miembros sin_corchetes
                      mapM_ (imprimir_arbol_parentisado (nivel+1)) miembros
            ' ' -> do let sin_espacio_inicial = tail arbol
                      imprimir_arbol_parentisado nivel sin_espacio_inicial
            ',' -> do let sin_coma_inicial = tail arbol
                      imprimir_arbol_parentisado nivel sin_coma_inicial
            _   -> do imprimir_espacios nivel
                      putStrLn arbol

-- Asume que se quitaron los corchetes exteriores
separar_miembros :: [Char] -> [[Char]]
separar_miembros arbol = let indices = separar_miembros_i arbol 0 0
                         in  obtener_valor $ recur ([], (indices, arbol))

-- Separa los miembros de un string que represente a una lista
separar_miembros_i :: [Char] -> Int -> Int -> [Int]
separar_miembros_i arbol num_corchetes num_index = 
    case arbol of
        ('[':xs) -> separar_miembros_i xs (num_corchetes+1) (num_index +1)
        (']':xs) -> separar_miembros_i xs (num_corchetes-1) (num_index +1)
        (',':xs) -> if num_corchetes == 0 
                        then num_index:(separar_miembros_i xs num_corchetes (num_index+1)) 
                        else separar_miembros_i xs num_corchetes (num_index+1)
        (_:xs) -> separar_miembros_i xs num_corchetes (num_index+1)
        [] -> []


main :: IO ()
main = do
    [nombre] <- getArgs
    source <- readFile nombre
    let lista = alexScanTokens source
    let arbol_sintactico = calc lista
    let arbol = show arbol_sintactico
    --putStrLn arbol
    --putStrLn "------------------------------------------"
    imprimir_arbol_parentisado 0 arbol

}




