module Main where
import Parser_Happy
import Data.Maybe
import Control.Monad
import Data.List
import System.Environment
import LexBot

-- Autor: Georvic Tur
-- Carnet: 12-11402
-- Correo: alexanderstower@gmail.com

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

--Devuelve el Just de la primera tabla que tenga la variable buscada
encontrar_en_alcance :: String -> PilaTabSimb -> Maybe TabSimbElemInfo
encontrar_en_alcance nombre pila = msum (map (encontrar_en_tabla_simb nombre) pila)


agregar_alcance :: TabSimb -> PilaTabSimb -> PilaTabSimb
agregar_alcance tabla pila = tabla:pila

sacar_ultimo_alcance :: PilaTabSimb -> PilaTabSimb
sacar_ultimo_alcance = tail


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
         Activation -> revisar_LIR lepts tipo listinstrob
         Deactivation -> revisar_LIR lepts tipo listinstrob
         Default -> revisar_LIR lepts tipo listinstrob
         Cond_Expr expr -> colapsar_lista_LD [revisar_Expr lepts tipo expr,revisar_LIR lepts tipo listinstrob]

--Lista de Instrucciones de Robot
revisar_LIR :: LEPTS_AST -> Tipo -> ListInstRob -> LEPTS_AST
revisar_LIR lepts tipo (ListInstRob_L lista_ins_rob) = colapsar_lista_LD $ map (revisar_IR lepts tipo) lista_ins_rob

--Instruccion de Robot
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


--Actualiza el estado con un nuevo error

insertar_error :: LEPTS_AST -> String -> LEPTS_AST
insertar_error (errores, pila) error_string = (error_string:errores, pila)

insertar_error_mensaje :: LEPTS_AST -> Tipo -> Tipo -> LEPTS_AST
insertar_error_mensaje lepts tipo_esperado tipo_real = insertar_error lepts ("Se esperaba un"++(imprimir_tipo tipo_esperado)++"y Se ha recibido un "++(imprimir_tipo tipo_real))



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
    putStrLn (show (head errores))
