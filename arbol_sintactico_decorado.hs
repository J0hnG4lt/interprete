module Main where
import Parser_Happy
import Data.Maybe
import Control.Monad
import Data.List
import System.Environment
import LexBot

---------------------------------------------------------
---------------------------------------------------------
{-
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
-}
---------------------------------------------------------
---------------------------------------------------------



data TabSimbElemInfo = Palabra (Maybe [Comp])
                    | Booleanoo (Maybe [Comp])
                    | Numero (Maybe [Comp])

instance Show TabSimbElemInfo where
    show (Palabra algo) = show algo
    show (Booleanoo algo) = show algo
    show (Numero algo) = show algo

construir_elem_info :: [Comp] -> Tipo -> TabSimbElemInfo
construir_elem_info lista tipo =
    case tipo of
        TInt -> Numero (Just lista)
        TBool -> Booleanoo (Just lista)
        TChar -> Palabra (Just lista)

declarar_elem_info tipo = 
    case tipo of
        TInt -> Numero Nothing
        TBool -> Booleanoo Nothing
        TChar -> Palabra Nothing
    
imprimir_tipo ::Tipo -> String
imprimir_tipo tipo = case tipo of
                        TBool -> " Booleano "
                        TInt -> " Entero "
                        TChar -> " Caracter "

construir_elem :: [Comp] -> Tipo -> String -> TabSimbElem
construir_elem lista tipo nombre 
    | ((length lista) /= 0) = (nombre, construir_elem_info lista tipo)
    | otherwise           = (nombre, declarar_elem_info tipo)

insertar_en_primera_tab_pila :: LEPTS_AST -> TabSimbElem -> LEPTS_AST
insertar_en_primera_tab_pila (errores, (tabla:tablas)) elem = (errores, ((elem:tabla):tablas))

insertar_Me :: LEPTS_AST -> Tipo -> LEPTS_AST
insertar_Me lepts tipo =
    case tipo of
        TInt -> insertar_en_primera_tab_pila lepts ("Me", Numero Nothing)
        TBool -> insertar_en_primera_tab_pila lepts ("Me", Booleanoo Nothing)
        TChar -> insertar_en_primera_tab_pila lepts ("Me", Palabra Nothing)

quitar_elemento :: LEPTS_AST -> String -> LEPTS_AST
quitar_elemento (errores, tabla:pila) nombre = (errores, (quitar_elem_ tabla nombre):pila)

quitar_elem_ :: TabSimb -> String -> TabSimb
quitar_elem_ todo@((nombre1, info):tabla) nombre2 = if nombre1 == nombre2
                                                then tabla
                                                else quitar_elem_ tabla nombre2
quitar_elem_ [] nombre = []

type TabSimbElem = (String, TabSimbElemInfo) -- Cambiar por DefRob

type TabSimb = [TabSimbElem]

encontrar_en_tabla_simb :: String -> TabSimb -> Maybe TabSimbElemInfo
encontrar_en_tabla_simb = lookup

type PilaTabSimb = [TabSimb]

insertar_elemento_en_tab_simb :: TabSimbElem -> TabSimb -> TabSimb
insertar_elemento_en_tab_simb elem tab = elem:tab



agregar_tabla :: LEPTS_AST -> TabSimb -> LEPTS_AST
agregar_tabla (errores, (tabla:lista_tablas)) nueva = (errores, ((nueva++tabla):lista_tablas))

encontrar_en_alcance :: String -> PilaTabSimb -> Maybe TabSimbElemInfo
encontrar_en_alcance nombre pila = msum (map (encontrar_en_tabla_simb nombre) pila)

agregar_alcance :: TabSimb -> PilaTabSimb -> PilaTabSimb
agregar_alcance tabla pila = tabla:pila

sacar_ultimo_alcance :: PilaTabSimb -> PilaTabSimb
sacar_ultimo_alcance = tail
{-
data Nodos = Nodo_Raiz AST | Nodo_LI ListInstrcs | Nodo_I Instrcs | Nodo_W While 
                 | Nodo_If IfCond | Nodo_Dir Dir | Nodo_Sec Secuen | Nodo_LD ListDecl 
                 | Nodo_DR DefRob | Nodo_T Tipo | Nodo_LId ListIdent | Nodo_Id Identific 
                 | Nodo_ChExp Char_Expr | Nodo_LC ListComp | Nodo_Comp Comp | Nodo_Cond Cond 
                 | Nodo_IR InstRob | Nodo_LIR ListInstRob | Nodo_V Var | Nodo_IC InstContr
                 | Nodo_Exp Expr | Nodo_Me Me
-}
type Lista_Errores = [String]

type LEPTS_AST = (Lista_Errores, PilaTabSimb)

lepts_ast_vacio :: LEPTS_AST
lepts_ast_vacio = ([],[[]])

insertar_en_primera_tab_simb :: LEPTS_AST -> TabSimbElem -> LEPTS_AST
insertar_en_primera_tab_simb (errores,(x:xs)) elem =  (errores,((insertar_elemento_en_tab_simb elem x):xs))

colapsar_lista :: [LEPTS_AST] -> LEPTS_AST
colapsar_lista lista = foldr op ([],[]) lista
    where op (lista_e_1,lista_p_1) (lista_e_2,lista_p_2) = ((lista_e_1 ++ lista_e_2), (lista_p_1 ++ lista_p_2)) -- Se debe igrnorar tabla?

{-
combinar_primeras_tablas :: LEPTS_AST -> LEPTS_AST -> LEPTS_AST
combinar_primeras_tablas (errores1,  tabla1:pila1) (errores2, tabla2:pila2) = (errores1++errores2, (tabla1++tabla2):)

colapsar_lista_2 :: [LEPTS_AST] -> LEPTS_AST
colapsar_lista_2 lista = foldr op ([],[]) lista
    where op (lista_e_1,lista_p_1) (lista_e_2,lista_p_2) = ((lista_e_1 ++ lista_e_2), (lista_p_1 ++ lista_p_2))
-}

revisar_arbol :: AST -> LEPTS_AST -> LEPTS_AST
revisar_arbol (Sec lista_instrcs) lepts = revisar_LI lista_instrcs lepts
revisar_arbol (Sec_Dec lista_decl lista_instrcs) lepts = let lepts1 = revisar_LD lista_decl lepts
                                                         in revisar_LI lista_instrcs lepts1


revisar_LI :: ListInstrcs -> LEPTS_AST -> LEPTS_AST
revisar_LI (ListInstrcs_L lista_instr) lepts = colapsar_lista (map ((flip revisar_I) lepts) lista_instr)

revisar_LD :: ListDecl -> LEPTS_AST -> LEPTS_AST
revisar_LD (ListDecl_L defrobs) lepts = revisar_redeclaracion (colapsar_lista (map ((flip revisar_DR) lepts) defrobs))

revisar_redeclaracion :: LEPTS_AST -> LEPTS_AST
revisar_redeclaracion lepts@(errores, tabla:pila) = if hay_duplicados_2 tabla 
                                                then insertar_error lepts "BOT redeclarado"
                                                else lepts

hay_duplicados_2 :: TabSimb -> Bool
hay_duplicados_2 tabla = foldr (||) False [(not $ null [identificador | (identificador, infor) <- tabla, identificador == nombre ]) | (nombre, inf) <- tabla ]
{-
hay_duplicados :: TabSimb -> Bool
hay_duplicados tabla = hay_duplicados_ (contar_duplicados tabla)

hay_duplicados_ :: (TabSimbElem, Int) -> Bool
hay_duplicados_ (_, 1) = False
hay_duplicados_ (_, _ ) = True
-}
revisar_I :: Instrcs -> LEPTS_AST -> LEPTS_AST
revisar_I (Instrcs_S secuen) lepts = revisar_Sec secuen lepts
revisar_I (Instrcs_W whil) lepts = revisar_W whil lepts
revisar_I (Instrcs_I ifcond) lepts = revisar_If ifcond lepts
revisar_I (Instrcs_Alcance ast) (errores, pila) = revisar_arbol ast (errores, (([]):pila))

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



revisar_LC :: ListComp -> LEPTS_AST -> Tipo -> LEPTS_AST       --
revisar_LC (ListComp_L listComp) lepts tipo = colapsar_lista $ map (revisar_Comp lepts tipo) listComp

revisar_Comp :: LEPTS_AST -> Tipo -> Comp -> LEPTS_AST
revisar_Comp lepts tipo (Comp cond listinstrob) = 
    case cond of
         Activation -> revisar_LIR lepts tipo listinstrob
         Deactivation -> revisar_LIR lepts tipo listinstrob
         Default -> revisar_LIR lepts tipo listinstrob
         Cond_Expr expr -> colapsar_lista [revisar_Expr lepts tipo expr,revisar_LIR lepts tipo listinstrob]

revisar_LIR :: LEPTS_AST -> Tipo -> ListInstRob -> LEPTS_AST
revisar_LIR lepts tipo (ListInstRob_L lista_ins_rob) = colapsar_lista $ map (revisar_IR lepts tipo) lista_ins_rob

revisar_IR :: LEPTS_AST -> Tipo -> InstRob -> LEPTS_AST
revisar_IR lepts tipo instrob = 
    case instrob of
        Almac expr -> revisar_Expr lepts tipo expr
        Colec expr -> revisar_Expr lepts tipo expr
        Solt expr -> revisar_Expr lepts tipo expr
        Mov dir expr -> revisar_Expr lepts tipo expr
        ES_Read (Identific_ (Var_C identif)) -> insertar_en_primera_tab_pila lepts (construir_elem [] tipo identif)
        ES_Empty_Read -> lepts
        ES_Empty_Send -> lepts
        Colec_empty -> lepts
        Mov_empty dir -> lepts
        Receive -> lepts

revisar_LId :: ListIdent -> [String]                           --
revisar_LId (ListIdent_V lista_ident) = map revisar_Identific lista_ident

revisar_Identific :: Identific -> String
revisar_Identific (Identific_ (Var_C nombre)) = nombre

revisar_Sec :: Secuen -> LEPTS_AST -> LEPTS_AST
revisar_Sec (Secuen lista_controladores) lepts = colapsar_lista $ map (revisar_IC lepts) lista_controladores

revisar_W :: While -> LEPTS_AST -> LEPTS_AST
revisar_W (While expr listinstrcs) lepts = colapsar_lista [revisar_Expr lepts TBool expr, revisar_LI listinstrcs lepts]

revisar_If :: IfCond -> LEPTS_AST -> LEPTS_AST
revisar_If (IfCond_Else expr listinstrcs1 listinstrcs2) lepts = colapsar_lista [revisar_Expr lepts TBool expr, revisar_LI listinstrcs1 lepts, revisar_LI listinstrcs2 lepts]
revisar_If (IfCond_Pass expr listinstrcs1) lepts = colapsar_lista [revisar_Expr lepts TBool expr, revisar_LI listinstrcs1 lepts]

--revisar_arbol :: AST -> LEPTS_AST -> LEPTS_AST


revisar_IC :: LEPTS_AST -> InstContr -> LEPTS_AST
revisar_IC lepts instcontr = 
            case instcontr of
                ActivateInst listident-> revisar_variables_declaradas lepts listident
                DeactivateInst listident -> revisar_variables_declaradas lepts listident
                AdvanceInst listident -> revisar_variables_declaradas lepts listident


revisar_variables_declaradas :: LEPTS_AST -> ListIdent -> LEPTS_AST
revisar_variables_declaradas lepts (ListIdent_V listident) = colapsar_lista ((map (revisar_variable_declarada lepts)) listident)


revisar_variable_declarada :: LEPTS_AST -> Identific -> LEPTS_AST
revisar_variable_declarada lepts@(errores, (tabla:pila)) (Identific_ (Var_C nombre)) =
            case (encontrar_en_alcance nombre pila) of
              Nothing -> insertar_error lepts ("No se encuentra la variable "++nombre)
              _ -> lepts



revisar_Expr :: LEPTS_AST -> Tipo -> Expr -> LEPTS_AST
revisar_Expr lepts@(errores, pila) tipo expr =
    case expr of
        Expr_Me_ me -> case encontrar_en_alcance "Me" pila of
                           Nothing -> insertar_error lepts "No se ha declarado Me"
                           Just algo -> lepts
        Equ expr1 expr2 -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
        NotEqu expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        And_ expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        Or_ expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        Not_ expr1 -> case tipo of
                            TBool -> revisar_Expr lepts tipo expr1
                            _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        MenorEqu expr1 expr2 -> case tipo of
                                    TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                    _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        Menor expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        MayorEqu expr1 expr2 -> case tipo of
                                    TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                    _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        Mayor expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        Variabl (Var_C nombre) -> case encontrar_en_alcance nombre pila of
                                        Nothing -> insertar_error lepts ("No se encuentra la variable "++nombre)
                                        _ -> lepts
        Booleano bool -> case tipo of
                            TBool -> lepts
                            _ -> insertar_error lepts ("Se esperaba un "++(imprimir_tipo tipo))
        Parentesis expr1 -> revisar_Expr lepts tipo expr1
        Suma expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo))
        Resta expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo))
        Divi expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo))
        Produ expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo))
        Modu expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo))
        Nega expr1 -> case tipo of
                                TInt -> revisar_Expr lepts TInt expr1
                                _ -> insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo))
        Numer algo -> case tipo of
                         TInt -> lepts
                         _ -> insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo))

insertar_error :: LEPTS_AST -> String -> LEPTS_AST
insertar_error (errores, pila) error_string = (error_string:errores, pila)



main :: IO ()
main = do
    [nombre] <- getArgs
    source <- readFile nombre
    let lista = alexScanTokens source
    let arbol_sintactico = calc lista
    putStrLn $ show arbol_sintactico
    let resultado = revisar_arbol arbol_sintactico ([],[[]])
    putStrLn (show resultado)
