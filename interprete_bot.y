{
{-# LANGUAGE NamedFieldPuns #-}
module Main where
import LexBot
import System.Environment
import Data.String
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import System.IO.Unsafe


}

-- Georvic Tur
-- Carnet: 12-11402
-- Correo: alexanderstower@gmail.com

-- NOTA: el codigo de analisis semantico se encuentra a partir de la linea 550


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
    |   Letra                       {Var_C $1}


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
    |   COLLECT AS Identificador_ '.'        {Colec (Identific_ $3)}
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
    |   Letra                          {Char_en_Expr $1}




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
            |   Colec Identific
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
    show Colec_empty = "(Colectar)"
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
        |       Char_en_Expr String
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
    show (Char_en_Expr ch) = "("++(show ch)++")"


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

-- Permite indentar
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



-------------------------------------------------------------------------------
-----------------Analisis de Contexto------------------------------------------
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-----------------Tipos de Datos------------------------------------------------
-------------------------------------------------------------------------------

--Lista de comportamientos de una variable con su tipo

data TabSimbElemInfo = Palabra (Maybe [Comp])
                    | Booleanoo (Maybe [Comp])
                    | Numero (Maybe [Comp])
                 deriving (Eq)

instance Show TabSimbElemInfo where
    show (Palabra algo) = show algo
    show (Booleanoo algo) = show algo
    show (Numero algo) = show algo

type TabSimbElem = (String, TabSimbElemInfo) -- Nombre de Variable y sus comportamientos

type TabSimb = [TabSimbElem] -- Tabla de Simbolos

type PilaTabSimb = [TabSimb] -- Pila de tablas para manerjar la instruccion de alcance

type Lista_Errores = [String] -- Lista de errores acumulados

type LEPTS_AST = (Lista_Errores, PilaTabSimb) -- Estado del programa


-------------------------------------------------------------------------------
-----------------funciones auxiliares------------------------------------------
-------------------------------------------------------------------------------



tipos_iguales :: Tipo -> TabSimbElemInfo -> Bool
tipos_iguales TInt (Numero algo) = True
tipos_iguales TBool (Booleanoo algo) = True
tipos_iguales TChar (Palabra algo) = True
tipos_iguales _ _ = False



-- Determina si alguna condicion default no esta al final de la lista de comportamientos

revisar_comportamiento_default :: LEPTS_AST -> LEPTS_AST
revisar_comportamiento_default lepts@(errores, (tabla:pila)) = if (and $ map comp_default_final elementos)
                                                               then lepts
                                                               else insertar_error lepts "El comportamiento Default debe estar al final."
                                                               where (nombres, elementos) = unzip tabla

comp_default_final :: TabSimbElemInfo -> Bool
comp_default_final elemento =
    case elemento of
        Palabra comp -> default_final comp
        Booleanoo comp -> default_final comp
        Numero comp -> default_final comp


default_final :: Maybe [Comp] -> Bool
default_final Nothing = True
default_final (Just lista) = let lista_comp = [condicion | (Comp condicion lista_instrucciones_robot) <- lista]
                                 ultimo = head lista_comp
                                 esta_en = elem Default lista_comp
                             in case ultimo of
                                  Default -> True
                                  _ -> not esta_en


-- Determina si hay comportamientos duplicados para una variable

revisar_comportamientos_duplicados :: LEPTS_AST -> LEPTS_AST
revisar_comportamientos_duplicados lepts@(errores, (tabla:pila)) = if (or $ map hay_comportamientos_duplicados elementos)
                                                             then insertar_error lepts "Hay comportamientos repetidos"
                                                             else lepts
                                                             where (nombres, elementos) = unzip tabla

hay_comportamientos_duplicados :: TabSimbElemInfo -> Bool
hay_comportamientos_duplicados elemento =
    case elemento of
        Palabra comp -> lista_comp_dups comp
        Booleanoo comp -> lista_comp_dups comp
        Numero comp -> lista_comp_dups comp


lista_comp_dups :: Maybe [Comp] -> Bool
lista_comp_dups Nothing = False
lista_comp_dups (Just lista) = let condiciones = [condicion | (Comp condicion lista_instrucciones_robot) <- lista]
                               in or [ (length (elemIndices elemento condiciones)) > 1 | elemento <- condiciones]


-- Determina si hay declaraciones repetidas

revisar_redeclaracion :: LEPTS_AST -> LEPTS_AST
revisar_redeclaracion lepts@(errores, tabla:pila) = if hay_duplicados_2 tabla 
                                                    then insertar_error lepts "BOT redeclarado"
                                                    else lepts

hay_duplicados_2 :: TabSimb -> Bool
hay_duplicados_2 tabla = or [ ((length (elemIndices elem nombres_de_variables)) > 1) | elem <- nombres_de_variables]
                         where (nombres_de_variables, ignorar) = unzip tabla
{-
hay_duplicados__2 :: TabSimb -> Bool
hay_duplicados__2 tabla = foldr (||) False [(not $ null [identificador | (identificador, infor) <- tabla, identificador == nombre ]) | (nombre, inf) <- tabla ]
-}


-- Determina si la variable habia sido declarada

revisar_variables_declaradas :: LEPTS_AST -> ListIdent -> LEPTS_AST
revisar_variables_declaradas lepts (ListIdent_V listident) = colapsar_lista ((map (revisar_variable_declarada lepts)) listident)


revisar_variable_declarada :: LEPTS_AST -> Identific -> LEPTS_AST
revisar_variable_declarada lepts@(errores, pila) (Identific_ (Var_C nombre)) =
            case (encontrar_en_alcance nombre pila) of
              Nothing -> insertar_error lepts ("No se encuentra la variable "++nombre)
              Just algo -> lepts



-- Construye el segundo elemento de la tupla que identifica una variable en la tabla

construir_elem_info :: [Comp] -> Tipo -> TabSimbElemInfo
construir_elem_info lista tipo =
    case tipo of
        TInt -> Numero (Just lista)
        TBool -> Booleanoo (Just lista)
        TChar -> Palabra (Just lista)

-- Construye la tupla que representa la variable

construir_elem :: [Comp] -> Tipo -> String -> TabSimbElem
construir_elem lista tipo nombre 
    | ((length lista) /= 0) = (nombre, construir_elem_info lista tipo)
    | otherwise           = (nombre, declarar_elem_info tipo)


-- declara una variable. No se guarda ningun comportamiento.

declarar_elem_info tipo = 
    case tipo of
        TInt -> Numero Nothing
        TBool -> Booleanoo Nothing
        TChar -> Palabra Nothing


-- printing

imprimir_tipo ::Tipo -> String
imprimir_tipo tipo = case tipo of
                        TBool -> " Booleano "
                        TInt -> " Entero "
                        TChar -> " Caracter "

imprimir_tipo_info :: TabSimbElemInfo -> String
imprimir_tipo_info info = case info of
                                        Numero algo -> "Entero"
                                        Booleanoo algo -> "Booleano"
                                        Palabra algo -> "Caracter"


-- Operaciones sobre la pila y las tablas


insertar_en_primera_tab_pila :: LEPTS_AST -> TabSimbElem -> LEPTS_AST
insertar_en_primera_tab_pila (errores, (tabla:tablas)) elem = (errores, ((elem:tabla):tablas))

insertar_Me :: LEPTS_AST -> Tipo -> LEPTS_AST
insertar_Me lepts tipo =
    case tipo of
        TInt -> insertar_en_primera_tab_pila lepts ("Me", Numero Nothing)
        TBool -> insertar_en_primera_tab_pila lepts ("Me", Booleanoo Nothing)
        TChar -> insertar_en_primera_tab_pila lepts ("Me", Palabra Nothing)

--Crea un alcance para collect

tabla_colec :: Tipo -> TabSimb
tabla_colec tipo = 
    case tipo of
        TInt -> [("Me", Numero Nothing)]
        TBool -> [("Me", Booleanoo Nothing)]
        TChar -> [("Me", Palabra Nothing)]

tabla_colec_var :: String -> Tipo -> TabSimbElem
tabla_colec_var nombre tipo =
    case tipo of
        TInt -> (nombre, Numero Nothing)
        TBool -> (nombre, Booleanoo Nothing)
        TChar -> (nombre, Palabra Nothing)

quitar_elemento :: LEPTS_AST -> String -> LEPTS_AST
quitar_elemento (errores, tabla:pila) nombre = (errores, (quitar_elem_ tabla nombre):pila)

quitar_elem_ :: TabSimb -> String -> TabSimb
quitar_elem_ todo@((nombre1, info):tabla) nombre2 = if nombre1 == nombre2
                                                then tabla
                                                else ((nombre1, info):(quitar_elem_ tabla nombre2))
quitar_elem_ [] nombre = []



encontrar_en_tabla_simb :: String -> TabSimb -> Maybe TabSimbElemInfo
encontrar_en_tabla_simb nombre ((identi, info):tabla) = if nombre == identi
                                                        then Just info
                                                        else encontrar_en_tabla_simb nombre tabla
encontrar_en_tabla_simb nombre [] = Nothing 



insertar_elemento_en_tab_simb :: TabSimbElem -> TabSimb -> TabSimb
insertar_elemento_en_tab_simb elem tab = elem:tab



agregar_tabla :: LEPTS_AST -> TabSimb -> LEPTS_AST
agregar_tabla (errores, (tabla:lista_tablas)) nueva = (errores, ((nueva++tabla):lista_tablas))
agregar_tabla (errores, []) nueva = (errores, [nueva])

--Devuelve el Just de la primera tabla que tenga la variable buscada
encontrar_en_alcance :: String -> PilaTabSimb -> Maybe TabSimbElemInfo
encontrar_en_alcance nombre pila = msum (map (encontrar_en_tabla_simb nombre) pila)


agregar_alcance :: TabSimb -> PilaTabSimb -> PilaTabSimb
agregar_alcance tabla pila = tabla:pila

sacar_ultimo_alcance :: LEPTS_AST -> LEPTS_AST
sacar_ultimo_alcance (errores, pila) = (errores,tail pila)


lepts_ast_vacio :: LEPTS_AST
lepts_ast_vacio = ([],[[]])

insertar_en_primera_tab_simb :: LEPTS_AST -> TabSimbElem -> LEPTS_AST
insertar_en_primera_tab_simb (errores,(x:xs)) elem =  (errores,((insertar_elemento_en_tab_simb elem x):xs))

-- Combina las listas de errores despues del execute

colapsar_lista :: [LEPTS_AST] -> LEPTS_AST
colapsar_lista lista = foldr op ([],[]) lista
    where op (lista_e_1,lista_p_1) (lista_e_2,lista_p_2) = ((lista_e_1 ++ lista_e_2), (lista_p_1))

-- Combina las listas de errores y las tablas de simbolos de las declaraciones

colapsar_lista_LD :: [LEPTS_AST] -> LEPTS_AST
colapsar_lista_LD lista = foldr op ([],[]) lista
    where op (lista_e_1,lista_p_1) (lista_e_2,lista_p_2) = ((lista_e_1 ++ lista_e_2), (combinar_primeras_tablas lista_p_1 lista_p_2))

combinar_primeras_tablas :: PilaTabSimb -> PilaTabSimb -> PilaTabSimb
combinar_primeras_tablas (tabla1:pila1) (tabla2:pila2) = ((tabla1++tabla2):pila1)
combinar_primeras_tablas pila [] = pila
combinar_primeras_tablas [] pila = pila




-- PRINCIPAL
-- Devuelve una tupla (errores_encontrados, pila_tabla_de_simbolos_global)

revisar_arbol :: AST -> LEPTS_AST -> LEPTS_AST
revisar_arbol (Sec lista_instrcs) lepts = revisar_LI lista_instrcs lepts
revisar_arbol (Sec_Dec lista_decl lista_instrcs) lepts = let lepts1 = revisar_LD lista_decl lepts
                                                         in revisar_LI lista_instrcs lepts1

-- Las funciones que siguen analizan los sub componentes del arbol

--Lista de Instrucciones
revisar_LI :: ListInstrcs -> LEPTS_AST -> LEPTS_AST
revisar_LI (ListInstrcs_L lista_instr) lepts = colapsar_lista (map ((flip revisar_I) lepts) lista_instr)

--Lista de Declaraciones
revisar_LD :: ListDecl -> LEPTS_AST -> LEPTS_AST
revisar_LD (ListDecl_L defrobs) lepts = revisar_comportamientos_duplicados $ revisar_comportamiento_default $ revisar_redeclaracion (colapsar_lista_LD (map ((flip revisar_DR) lepts) defrobs))



--Instruccion o secuencia
revisar_I :: Instrcs -> LEPTS_AST -> LEPTS_AST
revisar_I (Instrcs_S secuen) lepts = revisar_Sec secuen lepts
revisar_I (Instrcs_W whil) lepts = revisar_W whil lepts
revisar_I (Instrcs_I ifcond) lepts = revisar_If ifcond lepts
revisar_I (Instrcs_Alcance ast) (errores, pila) = revisar_arbol ast (errores, (([]):pila))

--Definicion de Robot
revisar_DR :: DefRob -> LEPTS_AST -> LEPTS_AST                --
revisar_DR (DefRob_Full tipo listIdent (ListComp_L listComp)) lepts = let identificadores = revisar_LId listIdent -- Chequear la Doble Declaración
                                                                          lepts1 = revisar_LC (ListComp_L listComp) (insertar_Me lepts tipo) tipo
                                                                          tabla_elems = map (construir_elem listComp tipo) identificadores
                                                                          lepts2 = agregar_tabla lepts1 tabla_elems
                                                                      in  quitar_elemento lepts2 "Me"
revisar_DR (DefRob_Empty tipo listIdent) lepts = let identificadores = revisar_LId listIdent
                                                     tabla_elems = map (construir_elem [] tipo) identificadores
                                                     lepts1 = agregar_tabla lepts tabla_elems
                                                 in  lepts1


--Lista de Comportamientos
revisar_LC :: ListComp -> LEPTS_AST -> Tipo -> LEPTS_AST       --
revisar_LC (ListComp_L listComp) lepts tipo = colapsar_lista_LD $ map (revisar_Comp lepts tipo) listComp

--Comportamiento
revisar_Comp :: LEPTS_AST -> Tipo -> Comp -> LEPTS_AST
revisar_Comp lepts tipo (Comp cond listinstrob) = 
    case cond of
         Activation -> sacar_ultimo_alcance (revisar_LIR lepts tipo listinstrob)
         Deactivation -> sacar_ultimo_alcance (revisar_LIR lepts tipo listinstrob)
         Default -> sacar_ultimo_alcance (revisar_LIR lepts tipo listinstrob)
         Cond_Expr expr -> sacar_ultimo_alcance (colapsar_lista_LD [revisar_Expr lepts tipo expr,revisar_LIR lepts tipo listinstrob])

--Lista de Instrucciones de Robot
revisar_LIR :: LEPTS_AST -> Tipo -> ListInstRob -> LEPTS_AST
revisar_LIR lepts tipo (ListInstRob_L lista_ins_rob) = 
                        let lepts0 = hay_collect lepts tipo (ListInstRob_L lista_ins_rob)
                            lepts1 = hay_read lepts0 tipo (ListInstRob_L lista_ins_rob)
                        in agregar_tabla (colapsar_lista_LD $ map (revisar_IR lepts1 tipo) (reverse lista_ins_rob)) (tabla_colec tipo)

--Si existe instruccion collect actualiza la pila
hay_collect :: LEPTS_AST -> Tipo -> ListInstRob -> LEPTS_AST
hay_collect lepts tipo (ListInstRob_L []) = lepts
hay_collect lepts tipo (ListInstRob_L (x:xs)) = case x of
                        (Colec iden) -> revisar_IR lepts tipo (Colec iden)
                        _ -> hay_collect lepts tipo (ListInstRob_L xs)

--Si existe instruccion read actualiza la pila
hay_read :: LEPTS_AST -> Tipo -> ListInstRob -> LEPTS_AST
hay_read lepts tipo (ListInstRob_L []) = lepts
hay_read lepts tipo (ListInstRob_L (x:xs)) = case x of
                        (ES_Read iden) -> revisar_IR lepts tipo (ES_Read iden)
                        _ -> hay_read lepts tipo (ListInstRob_L xs)

--Instruccion de Robot
revisar_IR :: LEPTS_AST -> Tipo -> InstRob -> LEPTS_AST
revisar_IR lepts tipo instrob = 
    case instrob of
        Almac expr -> revisar_Expr lepts tipo expr
        Colec iden ->  insertar_en_primera_tab_pila lepts (tabla_colec_var (revisar_Identific iden) tipo)
        Solt expr -> revisar_Expr lepts tipo expr
        Mov dir expr -> revisar_Expr lepts tipo expr
        ES_Read (Identific_ (Var_C identif)) -> insertar_en_primera_tab_pila lepts (construir_elem [] tipo identif)
        ES_Empty_Read -> lepts
        ES_Empty_Send -> lepts
        Colec_empty -> lepts
        Mov_empty dir -> lepts
        Receive -> lepts

--Lista de Identificadores
revisar_LId :: ListIdent -> [String]                           --
revisar_LId (ListIdent_V lista_ident) = map revisar_Identific lista_ident

--Identificador
revisar_Identific :: Identific -> String
revisar_Identific (Identific_ (Var_C nombre)) = nombre

--Secuencia de instrucciones de control
revisar_Sec :: Secuen -> LEPTS_AST -> LEPTS_AST
revisar_Sec (Secuen lista_controladores) lepts = colapsar_lista $ map (revisar_IC lepts) lista_controladores

--While
revisar_W :: While -> LEPTS_AST -> LEPTS_AST
revisar_W (While expr listinstrcs) lepts = colapsar_lista [revisar_Expr lepts TBool expr, revisar_LI listinstrcs lepts]

-- If
revisar_If :: IfCond -> LEPTS_AST -> LEPTS_AST
revisar_If (IfCond_Else expr listinstrcs1 listinstrcs2) lepts = colapsar_lista [revisar_Expr lepts TBool expr, revisar_LI listinstrcs1 lepts, revisar_LI listinstrcs2 lepts]
revisar_If (IfCond_Pass expr listinstrcs1) lepts = colapsar_lista [revisar_Expr lepts TBool expr, revisar_LI listinstrcs1 lepts]

--Instruccion de controlador
revisar_IC :: LEPTS_AST -> InstContr -> LEPTS_AST
revisar_IC lepts instcontr = 
            case instcontr of
                ActivateInst listident-> revisar_variables_declaradas lepts listident
                DeactivateInst listident -> revisar_variables_declaradas lepts listident
                AdvanceInst listident -> revisar_variables_declaradas lepts listident




--Expresion
revisar_Expr :: LEPTS_AST -> Tipo -> Expr -> LEPTS_AST
revisar_Expr lepts@(errores, pila) tipo expr =
    case expr of
        Expr_Me_ me -> case encontrar_en_alcance "Me" pila of
                           Nothing -> insertar_error lepts "No se puede usar Me fuera de las declaraciones."
                           Just algo -> lepts
        Equ expr1 expr2 -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
        NotEqu expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error_mensaje lepts TBool tipo
        And_ expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error_mensaje lepts TBool tipo
        Or_ expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error_mensaje lepts TBool tipo
        Not_ expr1 -> case tipo of
                            TBool -> revisar_Expr lepts tipo expr1
                            _ -> insertar_error_mensaje lepts TBool tipo
        MenorEqu expr1 expr2 -> case tipo of
                                    TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                    _ -> insertar_error_mensaje lepts TBool tipo
        Menor expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error_mensaje lepts TBool tipo
        MayorEqu expr1 expr2 -> case tipo of
                                    TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                    _ -> insertar_error_mensaje lepts TBool tipo
        Mayor expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error_mensaje lepts TBool tipo
        Variabl (Var_C nombre) -> case encontrar_en_alcance nombre pila of
                                        Nothing -> insertar_error lepts ("No se encuentra la variable "++nombre)
                                        Just infoo -> if (tipos_iguales tipo infoo) 
                                                      then lepts
                                                      else insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo)++"y Se ha recibido un "++(imprimir_tipo_info infoo))
        Booleano bool -> case tipo of
                            TBool -> lepts
                            _ -> insertar_error_mensaje lepts TBool tipo
        Parentesis expr1 -> revisar_Expr lepts tipo expr1
        Suma expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error_mensaje lepts TInt tipo
        Resta expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error_mensaje lepts TInt tipo
        Divi expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error_mensaje lepts TInt tipo
        Produ expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error_mensaje lepts TInt tipo
        Modu expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error_mensaje lepts TInt tipo
        Nega expr1 -> case tipo of
                                TInt -> revisar_Expr lepts TInt expr1
                                _ -> insertar_error_mensaje lepts TInt tipo
        Numer algo -> case tipo of
                         TInt -> lepts
                         _ -> insertar_error_mensaje lepts TInt tipo
        Char_en_Expr ch -> case tipo of
                         TChar -> lepts
                         _ -> insertar_error_mensaje lepts TChar tipo


--Actualiza el estado con un nuevo error

insertar_error :: LEPTS_AST -> String -> LEPTS_AST
insertar_error (errores, pila) error_string = (error_string:errores, pila)

insertar_error_mensaje :: LEPTS_AST -> Tipo -> Tipo -> LEPTS_AST
insertar_error_mensaje lepts tipo_esperado tipo_real = insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo_esperado)++"y Se ha recibido un "++(imprimir_tipo tipo_real))


data ValorBot = ValorBot {int :: Int, char :: String, bool :: Bool, errorBot :: String}
type Posxy = (Int, Int)
data EstadoBot = EstadoBot {nombre :: String, pos :: Posxy, valor :: ValorBot, comportamientos :: [Comp], tipo :: Tipo}

instance Show ValorBot where
    show val = (show $ int val) ++ (show $ bool val) ++ (show $ char val)

data Env = Env {variables :: [[EstadoBot]], matriz :: [(Posxy, ValorBot)]}

op_bot_int :: (Int -> Int -> Int) -> ValorBot -> ValorBot -> ValorBot
op_bot_int op val1 val2 = let valor1 = int val1
                              valor2 = int val2
                              valorb = (op) valor1 valor2
                          in ValorBot{int=valorb}

op_bot_bool :: (Bool -> Bool -> Bool) -> ValorBot -> ValorBot -> ValorBot
op_bot_bool op val1 val2 = let valor1 = bool val1
                               valor2 = bool val2
                               valorb = (op) valor1 valor2
                           in ValorBot{bool=valorb}


esta_en_lista_d :: [EstadoBot] -> String -> Bool
esta_en_lista_d (var:lista) nombreb = if ((nombre var) == nombreb)
                                      then True
                                      else esta_en_lista_d lista nombreb
esta_en_lista_d [] nombreb = False

encontrar_en_alcance_d :: [[EstadoBot]] -> String -> [EstadoBot]
encontrar_en_alcance_d (lista:pila) nombreb = if esta_en_lista_d lista nombreb
                                              then lista
                                              else encontrar_en_alcance_d pila nombreb
encontrar_en_alcance_d [] nombreb = []

encontrar_valor_bot :: [EstadoBot] -> String -> ValorBot
encontrar_valor_bot vars nombreb = if ((nombre var) == nombreb)
                                   then valor var
                                   else encontrar_valor_bot resto nombreb
                                   where var = head vars
                                         resto = tail vars
encontrar_valor_bot [] nombreb = ValorBot{errorBot="No se encuentra la variable"}

encontrar_estado_bot :: [EstadoBot] -> String -> EstadoBot
encontrar_estado_bot vars nombreb = if ((nombre var) == nombreb)
                                   then var
                                   else encontrar_estado_bot resto nombreb
                                   where var = head vars
                                         resto = tail vars
encontrar_estado_bot [] nombreb = EstadoBot{nombre=nombreb}--------------------------------


operar_expr_int :: (Int -> Int -> Int) ->  Expr -> Expr -> StateT Env IO (IO ValorBot)
operar_expr_int op expr1 expr2 = do env0 <- get
                                    let valorIO1 = (evalStateT $ eval_expr expr1) env0
                                    let valorIO2 = (evalStateT $ eval_expr expr2) env0
                                    return (liftM2 (op_bot_int (op)) (join valorIO1) (join valorIO2))

operar_expr_bool :: (Bool -> Bool -> Bool) ->  Expr -> Expr -> StateT Env IO (IO ValorBot)
operar_expr_bool op expr1 expr2 = do env0 <- get
                                     let valorIO1 = (evalStateT $ eval_expr expr1) env0
                                     let valorIO2 = (evalStateT $ eval_expr expr2) env0
                                     return (liftM2 (op_bot_bool (op)) (join valorIO1) (join valorIO2))

negar_bool :: ValorBot -> ValorBot
negar_bool val = ValorBot{bool= not (bool val)}

negar_int :: ValorBot -> ValorBot
negar_int val = ValorBot{int=  (0-(int val))}

eval_expr :: Expr -> StateT Env IO (IO ValorBot)
eval_expr (Numer algo) = do return (return ValorBot{int=algo})
eval_expr (Booleano algo) = do return (return ValorBot{bool=algo})
eval_expr (Char_en_Expr algo) = do return (return ValorBot{char=algo})
eval_expr (Variabl (Var_C nombre)) = do env0 <- get
                                        let valor = encontrar_valor_bot (encontrar_en_alcance_d (variables env0) nombre) nombre
                                        return (return valor)
eval_expr (Expr_Me_ me) = do env0 <- get
                             let valor = encontrar_valor_bot (encontrar_en_alcance_d (variables env0) "me") "me"
                             return (return valor)
eval_expr (Parentesis expr1) = do env0 <- get
                                  let val = (evalStateT $ eval_expr expr1) env0
                                  put env0
                                  return $ join val
eval_expr (Not_ expr1) = do env0 <- get
                            let valorIO = (evalStateT $ eval_expr expr1) env0
                            return (liftM negar_bool (join valorIO))
eval_expr (Nega expr1) = do env0 <- get
                            let valorIO = (evalStateT $ eval_expr expr1) env0
                            return (liftM negar_int (join valorIO))
eval_expr (Suma expr1 expr2) = operar_expr_int (+) expr1 expr2
eval_expr (Resta expr1 expr2) = operar_expr_int (-) expr1 expr2
eval_expr (Produ expr1 expr2) = operar_expr_int (*) expr1 expr2
eval_expr (Divi expr1 expr2) = operar_expr_int (div) expr1 expr2
eval_expr (Modu expr1 expr2) = operar_expr_int (mod) expr1 expr2
eval_expr (And_ expr1 expr2) = operar_expr_bool (&&) expr1 expr2
eval_expr (Or_ expr1 expr2) = operar_expr_bool (||) expr1 expr2
eval_expr (Mayor expr1 expr2) = operar_expr_bool (>) expr1 expr2
eval_expr (MayorEqu expr1 expr2) = operar_expr_bool (>=) expr1 expr2
eval_expr (Menor expr1 expr2) = operar_expr_bool (<) expr1 expr2
eval_expr (MenorEqu expr1 expr2) = operar_expr_bool (<=) expr1 expr2
eval_expr (Equ expr1 expr2) = operar_expr_bool (==) expr1 expr2
eval_expr (NotEqu expr1 expr2) = operar_expr_bool (/=) expr1 expr2


guardar_valor_bot_ :: [EstadoBot] -> String -> ValorBot -> [EstadoBot]
guardar_valor_bot_ (var:resto) nombreb valorb = if ((nombre var) == nombreb)
                                              then ((var{valor=valorb}):resto)
                                              else var:(guardar_valor_bot_ resto nombreb valorb)
guardar_valor_bot_ [] nombre valorb = []

guardar_valor_bot_pila :: [[EstadoBot]] -> String -> ValorBot -> [[EstadoBot]]
guardar_valor_bot_pila (vars:pila) nombreb valorb =
            if esta_en_lista_d vars nombreb
            then (guardar_valor_bot_ vars nombreb valorb):pila
            else (vars:(guardar_valor_bot_pila pila nombreb valorb))
guardar_valor_bot_pila [] nombreb valorb = []

guardar_valor_bot :: String -> ValorBot -> Env -> Env
guardar_valor_bot nombreb valorb env = let pila = variables env
                                           variables_nuevo = guardar_valor_bot_pila pila nombreb valorb
                                       in env{variables=variables_nuevo}

insertar_nueva_variable :: EstadoBot -> Env -> Env
insertar_nueva_variable estado env =
    let pila = variables env
    in case pila of
        (x:xs) -> let nueva_pila = ((estado:x):xs)
                  in env{variables=nueva_pila}
        _ -> let nueva_pila = [[estado]]
             in env{variables=nueva_pila}


obtener_pos_matriz :: String -> Env -> Posxy
obtener_pos_matriz nombreb env =
    let lista = encontrar_en_alcance_d (variables env) nombreb
        estado = encontrar_estado_bot lista nombreb
    in (pos estado)

obtener_valor_matriz :: Posxy -> [(Posxy, ValorBot)] -> ValorBot
obtener_valor_matriz poss ((posi,val):resto) =
    if poss == posi
    then val
    else obtener_valor_matriz poss resto
obtener_valor_matriz posi [] = ValorBot{errorBot="No hay valor en matriz"}

{-
actualizar_valor_matriz :: Posxy -> [(Posxy, ValorBot)] -> [(Posxy, ValorBot)]
actualizar_valor_matriz pos ((posi, val):xs) =
    if pos == posi
    then 
-}

guardar_variable_valor_matriz :: String -> String -> Env -> Env
guardar_variable_valor_matriz nombreb nombre_bot_actual env = 
    let pila = variables env
        posi = obtener_pos_matriz nombre_bot_actual env
        matrizz = matriz env
        valor_matriz_bot_actual = obtener_valor_matriz posi matrizz
        nueva_variable = EstadoBot{nombre=nombreb,
                                   valor=valor_matriz_bot_actual, 
                                   comportamientos=[], 
                                   pos=(0,0)}
        
    in insertar_nueva_variable nueva_variable env


modificar_matriz :: Posxy -> [(Posxy, ValorBot)] -> ValorBot -> [(Posxy, ValorBot)]
modificar_matriz posi ((posii,val):xs) vall = 
    if posii == posi
    then (posii, vall):xs
    else (posii,val):(modificar_matriz posi xs vall)
modificar_matriz posi [] vall = [(posi,vall)]


actualizar_valor_matriz :: Posxy -> Env -> ValorBot -> Env
actualizar_valor_matriz posi env valorb =
    let nueva_matriz = modificar_matriz posi (matriz env) valorb
    in env{matriz=nueva_matriz}

guardar_pos_bot_ :: [EstadoBot] -> String -> Dir -> ValorBot -> [EstadoBot]
guardar_pos_bot_ (var:resto) nombreb dir valorb = if ((nombre var) == nombreb)
                                              then  let nuevo_pos = mover_robot dir (pos var) valorb
                                                    in ((var{pos=nuevo_pos}):resto)
                                              else var:(guardar_pos_bot_ resto nombreb dir valorb)
guardar_pos_bot_ [] nombre dir valorb = []

guardar_pos_bot_pila :: [[EstadoBot]] -> String -> Dir -> ValorBot -> [[EstadoBot]]
guardar_pos_bot_pila (vars:pila) nombreb dir valorb =
            if esta_en_lista_d vars nombreb
            then (guardar_pos_bot_ vars nombreb dir valorb):pila
            else (vars:(guardar_pos_bot_pila pila nombreb dir valorb))
guardar_pos_bot_pila [] nombreb dir valorb = []

mover_robot :: Dir -> Posxy -> ValorBot -> Posxy
mover_robot dir (x,y) valorb =
    case dir of
        DDown -> (x, y - (int valorb))
        DUp -> (x, y + (int valorb))
        DLeft -> (x-(int valorb),y)
        DRight -> (x+(int valorb),y)


obtener_valor_segun_tipo :: String -> String -> [EstadoBot] -> ValorBot
obtener_valor_segun_tipo valorio nombreb lista =
    let tipo_bot = (tipo $ encontrar_estado_bot lista nombreb)
    in case tipo_bot of
        TInt -> ValorBot{int=(read valorio) :: Int}
        TChar -> ValorBot{char=valorio}
        TBool -> ValorBot{bool=(read valorio) :: Bool}


eval_IR :: InstRob -> String -> StateT Env IO (IO ValorBot)
eval_IR (Almac expr) nombre =
        do env0 <- get
           let valorb = (evalStateT $ eval_expr expr) env0
           let estado = ((liftM (flip (guardar_valor_bot nombre) env0) (join valorb))) 
           put (unsafePerformIO estado)
           return (join valorb)
eval_IR (Colec (Identific_ (Var_C nombreb))) nombre_bot_actual = 
    do env0 <- get
       let estado = guardar_variable_valor_matriz nombreb nombre_bot_actual env0
       put estado
       return $ return ValorBot{errorBot=""}
eval_IR (Solt expr) nombre_bot_actual =
    do env0 <- get
       let posi = obtener_pos_matriz nombre_bot_actual env0
       let valorb = (evalStateT $ eval_expr expr) env0
       let estado = ((liftM ( (actualizar_valor_matriz posi) env0) (join valorb))) 
       put (unsafePerformIO estado)
       return (join valorb)
eval_IR (Mov dir expr) nombre_bot_actual=
    do env0 <- get
       let valorb = (evalStateT $ eval_expr expr) env0
       let pila_n = ((liftM ( (guardar_pos_bot_pila (variables env0) nombre_bot_actual dir) ) (join valorb))) 
       put ( ((env0){variables=(unsafePerformIO pila_n)}))
       return (join valorb)
eval_IR (ES_Read (Identific_ (Var_C identif))) nombre_bot_actual =
    do env0 <- get
       liftIO $ putStrLn "Instroduzca un valor: "
       valorIO <- liftIO $ getLine
       --let valor_segun_tipo = obtener_valor_segun_tipo valorIO nombre_bot_actual (encontrar_en_alcance_d (variables env0) nombre_bot_actual)
       let nueva_var = EstadoBot{nombre=identif,
                                 valor=ValorBot{char=valorIO},
                                 pos=(0,0),
                                 comportamientos=[]}
       let nuevo_estado = insertar_nueva_variable nueva_var env0
       put nuevo_estado
       return $ return (ValorBot{errorBot=""})
eval_IR (ES_Empty_Read) nombreb =
    do env0 <- get
       liftIO $ putStrLn "Instroduzca un valor: "
       valorIO <- liftIO $ getLine
       let valor_b = obtener_valor_segun_tipo valorIO nombreb (encontrar_en_alcance_d (variables env0) nombreb)
       let pila_n = guardar_valor_bot_pila (variables env0) nombreb valor_b
       put (env0{variables=pila_n})
       return $ return ValorBot{errorBot=""}
eval_IR (ES_Empty_Send) nombreb =
    do env0 <- get
       let valor_bot = encontrar_valor_bot (encontrar_en_alcance_d (variables env0) nombreb) nombreb
       liftIO $ putStr "Valor: "
       liftIO $ putStrLn (show valor_bot)
       put env0
       return $ return ValorBot{errorBot=""}
eval_IR (Colec_empty) nombreb =
    do env0 <- get
       let posi = obtener_pos_matriz nombreb env0
       let valorr = obtener_valor_matriz posi (matriz env0)
       let pila = guardar_valor_bot_pila (variables env0) nombreb valorr
       let nuevo_env = env0{variables=pila}
       put nuevo_env
       return $ return ValorBot{errorBot=""}
eval_IR (Mov_empty dir) nombre_bot_actual =
    do env0 <- get
       let valorb = ValorBot{int=1}
       let pila_n = (( ( (guardar_pos_bot_pila (variables env0) nombre_bot_actual dir) ) ( valorb))) 
       put ( ((env0){variables=( pila_n)}))
       return (return valorb)
eval_IR (Receive) nombreb = eval_IR (ES_Empty_Send) nombreb


--Activate

eval_Activate :: [String] -> Env -> Env
eval_Activate (x:xs) env =
    let estado_bot = encontrar_estado_bot (encontrar_en_alcance_d (variables env) x) x
        nuevo_env = unsafePerformIO $ (execStateT (eval_Activar (encontrar_activar (comportamientos estado_bot)) x)) env
    in eval_Activate xs nuevo_env
eval_Activate [] env = env


encontrar_activar :: [Comp] -> ListInstRob
encontrar_activar ((x:xs)) =
    case x of
        ((Comp (Activation) listinstrob)) -> listinstrob
        ((Comp (Deactivation) listinstrob)) -> encontrar_activar ((xs))
        ((Comp (Default) listinstrob)) -> encontrar_activar ((xs))
        ((Comp (Cond_Expr expr) listinstrob)) -> encontrar_activar ((xs))
encontrar_activar [] = ListInstRob_L []

eval_Activar :: ListInstRob -> String -> StateT Env IO (IO ValorBot)
eval_Activar (ListInstRob_L (inst:lista)) nombre =
    do env0 <- get
       let env1 = (execStateT $ eval_IR inst nombre) env0
       put $ unsafePerformIO env1
       eval_Activar (ListInstRob_L lista) nombre
       return $ return ValorBot{errorBot=""}
eval_Activar (ListInstRob_L []) nombre =
    do env0 <- get
       return $ return ValorBot{errorBot=""}

--Eval Deactivate

eval_Deactivate :: [String] -> Env -> Env
eval_Deactivate (x:xs) env =
    let estado_bot = encontrar_estado_bot (encontrar_en_alcance_d (variables env) x) x
        nuevo_env = unsafePerformIO $ (execStateT (eval_Desactivar (encontrar_desactivar (comportamientos estado_bot)) x)) env
    in eval_Deactivate xs nuevo_env
eval_Deactivate [] env = env


encontrar_desactivar :: [Comp] -> ListInstRob
encontrar_desactivar ((x:xs)) =
    case x of
        ((Comp (Activation) listinstrob)) -> encontrar_desactivar ((xs))
        ((Comp (Deactivation) listinstrob)) -> listinstrob
        ((Comp (Default) listinstrob)) -> encontrar_desactivar ((xs))
        ((Comp (Cond_Expr expr) listinstrob)) -> encontrar_desactivar ((xs))
encontrar_desactivar [] = ListInstRob_L []

eval_Desactivar :: ListInstRob -> String -> StateT Env IO (IO ValorBot)
eval_Desactivar (ListInstRob_L (inst:lista)) nombre =
    do env0 <- get
       let env1 = (execStateT $ eval_IR inst nombre) env0
       put $ unsafePerformIO env1
       eval_Desactivar (ListInstRob_L lista) nombre
       return $ return ValorBot{errorBot=""}
eval_Desactivar (ListInstRob_L []) nombre =
    do env0 <- get
       return $ return ValorBot{errorBot=""}


--Eval Advance

eval_Advance :: [String] -> Env -> Env
eval_Advance (x:xs) env =
    let estado_bot = encontrar_estado_bot (encontrar_en_alcance_d (variables env) x) x
        nuevo_env = unsafePerformIO $ (execStateT (eval_Avanzar (encontrar_avanzar (comportamientos estado_bot) env) x)) env
    in eval_Advance xs nuevo_env
eval_Advance [] env = env


encontrar_avanzar :: [Comp] -> Env -> ListInstRob
encontrar_avanzar ((x:xs)) env =
    case x of
        ((Comp (Activation) listinstrob)) -> encontrar_avanzar ((xs)) env
        ((Comp (Deactivation) listinstrob)) -> encontrar_avanzar ((xs)) env
        ((Comp (Default) listinstrob)) -> encontrar_avanzar ((xs)) env
        ((Comp (Cond_Expr expr) listinstrob)) -> if (bool (unsafePerformIO $ join $ (evalStateT $ eval_expr expr) env))
                                                 then listinstrob
                                                 else encontrar_avanzar xs env
encontrar_avanzar [] env = ListInstRob_L []

eval_Avanzar :: ListInstRob -> String -> StateT Env IO (IO ValorBot)
eval_Avanzar (ListInstRob_L (inst:lista)) nombre =
    do env0 <- get
       let env1 = (execStateT $ eval_IR inst nombre) env0
       put $ unsafePerformIO env1
       eval_Avanzar (ListInstRob_L lista) nombre
       return $ return ValorBot{errorBot=""}
eval_Avanzar (ListInstRob_L []) nombre =
    do env0 <- get
       return $ return ValorBot{errorBot=""}


--Eval IC

eval_IC :: InstContr -> StateT Env IO (IO ValorBot)
eval_IC (ActivateInst listident) =
    do env0 <- get
       let identificadores = obtener_identificadores listident
       let nuevo_estado = eval_Activate identificadores env0
       put nuevo_estado
       return $ return ValorBot{errorBot=""}
eval_IC (DeactivateInst listident) =
    do env0 <- get
       let identificadores = obtener_identificadores listident
       let nuevo_estado = eval_Deactivate identificadores env0
       put nuevo_estado
       return $ return ValorBot{errorBot=""}
eval_IC (AdvanceInst listident) =
    do env0 <- get
       let identificadores = obtener_identificadores listident
       let nuevo_estado = eval_Advance identificadores env0
       put nuevo_estado
       return $ return ValorBot{errorBot=""}

obtener_identificadores :: ListIdent -> [String]
obtener_identificadores (ListIdent_V ((Identific_ (Var_C nombre)):xs)) = nombre : (obtener_identificadores (ListIdent_V xs)) 
obtener_identificadores (ListIdent_V []) = []

{-
revisar_LI :: ListInstrcs -> LEPTS_AST -> LEPTS_AST
revisar_LI (ListInstrcs_L lista_instr) lepts = colapsar_lista (map ((flip revisar_I) lepts) lista_instr)
-}


eval_LI :: ListInstrcs -> StateT Env IO (IO ValorBot)
eval_LI (ListInstrcs_L (x:xs)) =
    do env0 <- get
       let nuevo_estado = (execStateT $ eval_I x) env0
       put $ unsafePerformIO nuevo_estado
       eval_LI (ListInstrcs_L (xs))
       return $ return ValorBot{errorBot=""}
eval_LI (ListInstrcs_L []) = return $ return ValorBot{errorBot=""}



eval_Sec :: Secuen -> StateT Env IO (IO ValorBot)
eval_Sec (Secuen (x:xs)) =
    do env0 <- get
       let nuevo_estado = (execStateT $ eval_IC x) env0
       put $ unsafePerformIO nuevo_estado
       eval_Sec (Secuen (xs))
       return $ return ValorBot{errorBot=""}
eval_Sec (Secuen []) = return $ return ValorBot{errorBot=""}


eval_W :: While -> StateT Env IO (IO ValorBot)
eval_W (While expr listinstrcs) =
    do env0 <- get
       let valorIO1 = (evalStateT $ eval_expr expr) env0
       if (bool (unsafePerformIO $ join valorIO1))
       then do let nuevo_estado = (execStateT $ eval_LI listinstrcs) env0
               put $ unsafePerformIO nuevo_estado
               eval_W (While expr listinstrcs)
               return $ return ValorBot{errorBot=""}
       else do return $ return ValorBot{errorBot=""}

eval_If :: IfCond -> StateT Env IO (IO ValorBot)
eval_If (IfCond_Else expr listinstrcs1 listinstrcs2) =
    do env0 <- get
       let valorIO1 = (evalStateT $ eval_expr expr) env0
       if (bool (unsafePerformIO $ join valorIO1))
       then do let nuevo_estado = (execStateT $ eval_LI listinstrcs1) env0
               put $ unsafePerformIO nuevo_estado
               return $ return ValorBot{errorBot=""}
       else do let nuevo_estado = (execStateT $ eval_LI listinstrcs2) env0
               put $ unsafePerformIO nuevo_estado
               return $ return ValorBot{errorBot=""}

eval_Arbol :: AST -> StateT Env IO (IO ValorBot)
eval_Arbol (Sec lista_instrcs) =
    do env0 <- get
       eval_LI lista_instrcs
eval_Arbol (Sec_Dec lista_decl lista_instrcs) =
    do env0 <- get
       let nuevo_estado = (execStateT $ preparar_LD (lista_decl)) env0
       put $ unsafePerformIO nuevo_estado
       eval_LI lista_instrcs

preparar_LD :: ListDecl -> StateT Env IO (IO ValorBot)
preparar_LD (ListDecl_L (x:defrobs)) =
    do env0 <- get
       let nuevo_estado = (execStateT $ preparar_DR x) env0
       put $ unsafePerformIO nuevo_estado
       preparar_LD (ListDecl_L defrobs)
preparar_LD (ListDecl_L []) = return $ return ValorBot{errorBot=""}

preparar_DR :: DefRob -> StateT Env IO (IO ValorBot)
preparar_DR (DefRob_Full tipob (ListIdent_V ((Identific_ ( (Var_C nombreb))):listIdent)) (ListComp_L listComp)) =
    do env0 <- get
       let nuevo_estado_bot = EstadoBot{nombre=nombreb,
                                        tipo=tipob,
                                        valor=ValorBot{errorBot=""},
                                        pos=(0,0),
                                        comportamientos=listComp}
       let nuevo_env = insertar_nueva_variable nuevo_estado_bot env0
       put nuevo_env
       preparar_DR (DefRob_Full tipob (ListIdent_V (listIdent)) (ListComp_L listComp))
preparar_DR (DefRob_Full tipob (ListIdent_V ([])) (ListComp_L listComp)) =
                return $ return ValorBot{errorBot=""}
preparar_DR (DefRob_Empty tipob (ListIdent_V ((Identific_ ( (Var_C nombreb))):listIdent))) =
    do env0 <- get
       let nuevo_estado_bot = EstadoBot{nombre=nombreb,
                                        tipo=tipob,
                                        valor=ValorBot{errorBot=""},
                                        pos=(0,0),
                                        comportamientos=[]}
       let nuevo_env = insertar_nueva_variable nuevo_estado_bot env0
       put nuevo_env
       preparar_DR (DefRob_Empty tipob (ListIdent_V (listIdent)))
preparar_DR (DefRob_Empty tipob (ListIdent_V ([]))) =
                return $ return ValorBot{errorBot=""}


eval_I :: Instrcs -> StateT Env IO (IO ValorBot)
eval_I (Instrcs_S secuen) =
    do env0 <- get
       let nuevo_estado = (execStateT $ eval_Sec secuen) env0
       put $ unsafePerformIO nuevo_estado
       return $ return ValorBot{errorBot=""}
eval_I (Instrcs_W whil) =
    do env0 <- get
       let nuevo_estado = (execStateT $ eval_W whil) env0
       put $ unsafePerformIO nuevo_estado
       return $ return ValorBot{errorBot=""}
eval_I (Instrcs_I ifcond) =
    do env0 <- get
       let nuevo_estado = (execStateT $ eval_If ifcond) env0
       put $ unsafePerformIO nuevo_estado
       return $ return ValorBot{errorBot=""}
eval_I (Instrcs_Alcance ast) =
    do env0 <- get
       let nuevo_estado = (execStateT $ eval_Arbol ast) env0
       put $ unsafePerformIO nuevo_estado
       return $ return ValorBot{errorBot=""}


{-
guardar_valor_bot_pila :: [[EstadoBot]] -> String -> ValorBot -> [[EstadoBot]]
obtener_valor_matriz :: Posxy -> [(Posxy, ValorBot)] -> ValorBot
obtener_pos_matriz :: String -> Env -> Posxy
-}
{-
    case instrob of
        Receive -> lepts
-}

main :: IO ()
main = do
    [nombre] <- getArgs
    source <- readFile nombre
    let lista = alexScanTokens source
    let arbol_sintactico = calc lista
    let arbol = show arbol_sintactico
    --putStrLn arbol
    imprimir_arbol_parentisado 0 arbol
    let (errores, pila) = revisar_arbol arbol_sintactico ([],[[]])
    putStrLn "\n\n"
    if null errores 
        then do putStrLn "No se encontraron errores semanticos."
                x <- (evalStateT (eval_Arbol arbol_sintactico)) Env{variables=[], matriz=[]}
                putStrLn "Fin"
        else putStrLn (show (last errores)) --El primer error se encuentra al final
    putStrLn " "

}




