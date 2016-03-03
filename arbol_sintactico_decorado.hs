module Main where
import parser_happy

data TipoVar = Palabra__ | Booleano__ | Numero__

data TabSimbElemInfo = Palabra (Maybe ListComp)
                    | Booleano (Maybe ListComp)
                    | Numero (Maybe ListComp)

construir_elem_info :: ListComp -> Tipo -> TabSimbElemInfo
construir_elem_info lista tipo =
    case tipo of
        TInt -> Numero (Just lista)
        TBool -> Booleano (Just lista)
        TChar -> Palabra (Just lista)

declarar_elem_info tipo = 
    case tipo of
        TInt -> Numero None
        Tbool -> Booleano None
        TChar -> Palabra None

imprimir_tipo ::Tipo -> String
imprimir_tipo tipo = case tipo of
                        TBool -> " Booleano "
                        TInt -> " Entero "
                        TChar -> " Caracter "

construir_elem :: ListComp -> Tipo -> String -> TabSimbElem
construir_elem lista tipo nombre 
    | (length lista /= 0) = (nombre, construir_elem_info lista tipo)
    | otherwise           = (nombre, declarar_elem_info tipo)

insertar_en_primera_tab_pila :: LEPTS_AST -> TabSimbElem -> LEPTS_AST
insertar_en_primera_tab_pila (errores, (tabla:tablas)) elem = (errores, ((elem:tabla):tablas))

type TabSimbElem = (String, TabSimbElemInfo) -- Cambiar por DefRob

type TabSimb = [TabSimbElem]

encontrar_en_tabla_simb :: String -> TabSimb -> Maybe TabSimbElemInfo
encontrar_en_tabla = lookup

type PilaTabSimb = [TabSimb]

insertar_elemento_en_tab_simb :: TabSimbElem -> TabSimb -> TabSimb
insertar_elemento_en_tab_simb elem tab = elem:tab



agregar_tabla :: LEPTS_AST -> TabSimb -> LEPTS_AST
agregar_tabla (errores, (tabla:lista_tablas)) nueva = (errores, ((nueva++tabla):lista_tablas))

encontrar_en_alcance :: String -> PilaTabSimb -> Maybe TabSimbElemInfo
encontrar_en_alcance nombre pila = msum (map (encontrar_en_tabla nombre) pila)

agregar_alcance :: TabSimb -> PilaTabSimb -> PilaTabSimb
agregar_alcance tabla pila = tabla:pila

sacar_ultimo_alcance :: PilaTabSimb -> PilaTabSimb
sacar_ultimo_alcance = tail

data Nodos = Nodo_Raiz AST | Nodo_LI ListInstrcs | Nodo_I Instrcs | Nodo_W While 
                 | Nodo_If IfCond | Nodo_Dir Dir | Nodo_Sec Secuen | Nodo_LD ListDecl 
                 | Nodo_DR DefRob | Nodo_T Tipo | Nodo_LId ListIdent | Nodo_Id Identific 
                 | Nodo_ChExp Char_Expr | Nodo_LC ListComp | Nodo_Comp Comp | Nodo_Cond Cond 
                 | Nodo_IR InstRob | Nodo_LIR ListInstRob | Nodo_V Var | Nodo_IC InstContr
                 | Nodo_Exp Expr | Nodo_Me Me

type Lista_Errores = [String]

type LEPTS_AST = (Lista_Errores, PilaTabSimb)

insertar_en_primera_tab_simb :: LEPTS_AST -> TabSimbElem -> LEPTS_AST
insertar_en_primera_tab_simb (errores,(x:xs)) elem =  (errores,((insertar_elemento_en_tab_simb elem x):xs))

colapsar_lista :: [LEPTS_AST] -> LEPTS_AST
colapsar_lista lista = foldr op ([],[]) lista
    where op (lista_e_1,lista_p_1) (lista_e_2,lista_p_2) = ((lista_e_1 ++ lista_e_2), (lista_p_1 ++ lista_p_2))

revisar_arbol :: AST -> LEPTS_AST -> LEPTS_AST
revisar_arbol (Sec lista_instrcs) lepts = revisar_LI lista_instrcs lepts
revisar_arbol (Sec_Dec lista_decl lista_instrcs) lepts = let lepts1 = revisar_LD lista_decl lepts
                                                         in revisar_LI lista_instrcs lepts1


revisar_LI :: ListInstrcs -> LEPTS_AST -> LEPTS_AST
revisar_LI (ListInstrcs_L lista_instr) lepts = colapsar_lista (map revisar_I lista_instrcs)

revisar_LD :: ListDecl -> LEPTS_AST -> LEPTS_AST
revisar_LD (ListDecl_L defrobs) = colapsar_lista (map revisar_DR defrobs)

revisar_I :: Instrcs -> LEPTS_AST -> LEPTS_AST
revisar_I (Instrcs_S secuen) lepts = revisar_Sec secuen lepts
revisar_I (Instrcs_W whil) lepts = revisar_W whil lepts
revisar_I (Instrcs_I ifcond) lepts = revisar_If ifcond lepts
revisar_I (Instrcs_Alcance ast) lepts = revisar_arbol ast lepts

revisar_DR :: DefRob -> LEPTS_AST -> LEPTS_AST                --
revisar_DR (DefRob_Full tipo listIdent listComp) lepts = let identificadores = revisar_LId listIdent
                                                             lepts1 = revisar_LC listComp lepts tipo
                                                             tabla_elems = map (construir_elem listComp tipo) identificadores
                                                             lepts2 = agregar_tabla lepts1 tabla_elems
                                                         in  lepts2
revisar_DR (DefRob_Empty tipo listIdent) lepts = let identificadores = revisar_LId listIdent
                                                     tabla_elems = map (construir_elem [] tipo) identificadores
                                                     lepts1 = agregar_tabla lepts tabla_elems
                                                 in  lepts1



revisar_LC :: ListComp -> LEPTS_AST -> Tipo -> LEPTS_AST       --
revisar_LC listComp lepts tipo = colapsar_lista map (revisar_Comp lepts tipo) listComp

revisar_Comp :: LEPTS_AST -> Tipo -> Comp -> LEPTS_AST
revisar_Comp lepts tipo (Comp cond listinstrob) = 
    case cond of
         Activation -> revisar_LIR lepts tipo listinstrob
         Deactivation -> revisar_LIR tipo lepts listinstrob
         Default -> revisar_LIR lepts tipo listinstrob
         Cond_Expr expr -> colapsar_lista [revisar_Expr lepts tipo expr,revisar_LIR lepts tipo listinstrob]

revisar_LIR :: LEPTS_AST -> Tipo -> ListInstRob -> LEPTS_AST
revisar_LIR lepts tipo lista_ins_rob = colapsar_lista map (revisar_IR lepts tipo) lista_ins

revisar_IR :: LEPTS_AST -> Tipo -> InstRob -> LEPTS_AST
revisar_IR lepts tipo instrob = 
    case instrob of
        Almac expr -> revisar_Expr lepts tipo expr
        Colec expr -> revisar_Expr lepts tipo expr
        Solt expr -> revisar_Expr lepts tipo expr
        Mov dir expr -> revisar_Expr tipo lepts expr
        ES_Read identif -> insertar_en_primera_tab_pila lepts (construir_elem [] tipo identif)
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
revisar_Sec (Secuen lista_controladores) lepts = colapsar_lista map (revisar_IC lepts) lista_controladores

revisar_W :: While -> LEPTS_AST -> LEPTS_AST


revisar_If :: IfCond -> LEPTS_AST -> LEPTS_AST

revisar_arbol :: AST -> LEPTS_AST -> LEPTS_AST

revisar_IC :: LEPTS_AST -> InstContr -> LEPTS_AST
revisar_IC lepts instcontr = 
            case instcontr of
                ActivateInst listident-> revisar_variables_declaradas lepts listIdent
                DeactivateInst listident -> revisar_variables_declaradas lepts listIdent
                AdvanceInst listident -> revisar_variables_declaradas lepts listIdent


revisar_variables_declaradas :: LEPTS_AST -> ListIdent -> LEPTS_AST
revisar_variables_declaradas lepts listident = colapsar_lista ((map (revisar_variable_declarada lepts)) listident)


revisar_variable_declarada :: LEPTS_AST -> Identific -> LEPTS_AST
revisar_variable_declarada lepts@(errores, (tabla:pila)) (Identific_ (Var_C nombre)) =
            case (encontrar_en_tabla_simb nombre tabla) of
              None -> insertar_error lepts "No se encuentra la variable "++nombre
              _ -> lepts



revisar_Expr :: LEPTS_AST -> Expr -> LEPTS_AST
revisar_Expr lepts@(errores, pila) tipo expr =
    case expr of
        Expr_Me_ me -> case encontrar_en_alcance Me lepts of
                           None -> insertar_error lepts "No se ha declarado Me"
                           Just algo -> lepts
        Equ expr1 expr2 -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
        NotEqu expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        And_ expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        Or_ expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts tipo expr1,revisar_Expr lepts tipo expr2]
                                _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        Not_ expr1 -> case tipo of
                            TBool -> revisar_Expr lepts tipo expr1
                            _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        MenorEqu expr1 expr2 -> case tipo of
                                    TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                    _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        Menor expr1 expr2 -> case tipo of
                                TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        MayorEqu expr1 expr2 -> case tipo of
                                    TBool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                    _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        Mayor expr1 expr2 -> case tipo of
                                Tbool -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        Variabl (Var_C nombre) -> case encontrar_en_alcance nombre lepts of
                                        None -> insertar_error lepts ("No se encuentra la variable "++nombre)
                                        _ -> lepts
        Booleano bool -> case tipo of
                            TBool -> lepts
                            _ -> insertar_error lepts "Se esperaba un "++(imprimir_tipo tipo)
        Parentesis expr1 -> revisar_Expr lepts tipo expr1
        Suma expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts "Se esperaba un"++(imprimir_tipo tipo)
        Resta expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts "Se esperaba un"++(imprimir_tipo tipo)
        Divi expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts "Se esperaba un"++(imprimir_tipo tipo)
        Produ expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts "Se esperaba un"++(imprimir_tipo tipo)
        Modu expr1 expr2 -> case tipo of
                                TInt -> colapsar_lista [revisar_Expr lepts TInt expr1,revisar_Expr lepts TInt expr2]
                                _ -> insertar_error lepts "Se esperaba un"++(imprimir_tipo tipo)
        Nega expr1 -> case tipo of
                                TInt -> revisar_Expr lepts TInt expr1
                                _ -> insertar_error lepts "Se esperaba un"++(imprimir_tipo tipo)
        Numer -> case tipo of
                         TInt -> lepts
                         _ -> insertar_error lepts "Se esperaba un"++(imprimir_tipo tipo)

insertar_error :: [Error] -> String -> [Error]

main :: IO ()
main = do
    [nombre] <- getArgs
    source <- readFile nombre
    let lista = alexScanTokens source
    let arbol_sintactico = calc lista
    let arbol = show arbol_sintactico
    imprimir_arbol_parentisado 0 arbol
