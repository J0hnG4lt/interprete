{
module Main where
import LexBot (alexScanTokens)
}

-- Georvic Tur
-- 12-11402

%name calc            -- [ Token ] -> T
%tokentype { Token }
%error { parseError } -- Implementar Funcion parseError

%left "/\\" "\\/"
%left '=' "<=" ">=" "/=" '<' '>'
%left '*' '/'
%left '%'
%left NEG
    -- Definicion de Token

%token
    "create"      {TkCreate}
    "execute"     {TkExecute}
    "end"         {TkEnd}
    "bot"         {TkBot}
    "int"         {TkInt}
    "bool"        {TkBool}
    "char"        {TkChar}
    "store"       {TkStore}
    "receive"     {TkReceive}
    "on"          {TkOn}
    "activate"    {TkActivate}
    "advance"     {TkAdvance}
    "deactivate"  {TkDeactivate}
    "if"          {TkIf}
    "else"        {TkElse}
    "while"       {TkWhile}
    "collect"     {TkCollect}
    "as"          {TkAs}
    "drop"        {TkDrop}
    "up"          {TkUp}
    "down"        {TkDown}
    "left"        {TkLeft}
    "right"       {TkRight}
    "read"        {TkRead}
    "send"        {TkSend}
    "activation"  {TkActivation}
    "deactivation"    {TkDeactivation}
    "default"         {TkDefault}
    "me"              {TkMe}
    '+'             {TkSuma}
    '-'             {TkResta}
    '*'             {TkMult}
    '/'             {TkDiv}
    '%'             {TkMod}
    "/\\"            {TkConjuncion}
    "\/"            {TkDisyuncion}
    '~'             {TkNegacion}
    '<'             {TkMenor}
    "<="            {TkMenorIgual}
    '>'             {TkMayor}
    ">="            {TkMayorIgual}
    '='             {TkIgual}
    "/="            {TkDesigualdad}
    Variable             {TkIdent $$}
    Constante           {TkNum $$}
    "true"            {TkTrue}
    "false"           {TkFalse}
    Letra           {TkCaracter $$}
    ','             {TkComa}
    '.'             {TkPunto}
    ':'             {TkDosPuntos}
    '('             {TkParAbre}
    ')'             {TkParCierra}


%%

Secuencia : "create" Lista_de_Declaraciones "execute" Instrucciones "end"      {Sec $2 $4}
    |   "execute" Instruccion_de_Controlador "end"                                          {Sec [] $2}

Definicion_de_Robot : Tipo "bot" Lista_de_Identificadores Lista_de_Comportamientos "end"    {DefRob $1 $3 $4}
    |   Tipo "bot" Lista_de_Identificadores "end"                                           {DefRob $1 $3 []}

Lista_de_Declaraciones : Lista_de_Declaraciones Definicion_de_Robot     {ListDecl $1 $2}

Tipo : Tipo Int      {Tipo $1}
    | Tipo Bool      {Tipo $1}
    | Tipo Char      {Tipo $1}

Int : {Int}
Bool : {Bool}
Char : {Char}

Lista_de_Comportamientos : Lista_de_Comportamientos Comportamiento      {ListComp $1 $2}
    |   Comportamiento                                                  {ListComp [] $1}

Comportamiento : "on" Condicion ':' Instruccion_de_Robot "end"          {Comp $2 $4}

Lista_de_Identificadores : Lista_de_Identificadores ',' Variable        {ListIdent $1 $3}



Condicion : Activation      {Cond Activation}
    |   Deactivation        {Cond Deactivation}
    |   Default             {Cond Default}
    |   Expresion           {Cond $1}  -- Esto debe estar mal

Activation : {Activation}
Deactivation : {Deactivation}
Default : {Default}


Instruccion_de_Controlador : "activate" Lista_de_Identificadores        {InstContr ActivateInst $2}
    |   "advance" Lista_de_Identificadores                              {InstContr AdvanceInst $2}
    |   "deactivate" Lista_de_Identificadores                           {InstContr DeactivateInst $2}


Instrucciones : Secuenciacion                   {Instrcs $1}
    |       Iteracion_Indeterminada             {Instrcs $1}
    |       Condicional                         {Instrcs $1}


Secuenciacion : Secuenciacion Instruccion_de_Controlador        {Secuen $1 $2}
    |   Instruccion_de_Controlador                              {Secuen [] $1}

Condicional : "if" Bool Secuenciacion "else" Secuenciacion "end"        {IfCond $2 $3 $5}
    |   "if" Bool Secuenciacion "end"                                   {IfCond $2 $3 []}

Iteracion_Indeterminada : "while" Bool ':' Secuenciacion "end"          {While $2 $4}


Instruccion_de_Robot : Almacenado '.'       {InstRob $1}
    |   Coleccion '.'                       {InstRob $1}
    |   Soltado '.'                         {InstRob $1}
    |   Movimiento '.'                      {InstRob $1}
    |   Entrada_y_Salida '.'                {InstRob $1}
--    |   Secuenciacion_Inst '.'              {InstRob $1}


Coleccion : "collect" "as" Identificador      {Colec $3}
    | "collect"                               {Colec []}

Identificador : Variable                      {Var $1}

Soltado : "drop" Expresion                      {Solt $2}

Movimiento : Direccion Expresion                {Mov $1 $2}
    | Direccion                                 {Mov $1 []}

Direccion : Left        {Dir $1}
    |   Right           {Dir $1}
    |   Up              {Dir $1}
    |   Down            {Dir $1}

Left : {Left}
Up : {Up}
Right : {Right}
Down : {Down}


Entrada_y_Salida : "read" "as" Identificador        {ES Read $3}
    |   "read"                                      {ES Read []}
    |   "send"                                      {ES Send []}

Almacenado : "store" Expresion                  {Almac $2}

Expresion : Expresion_Bool                          {Expr $1}
    |   Expresion_Num                               {Expr $1}

Expresion_Num : Expresion_Num '+' Expresion_Num     {Suma $1 $3}
    |   Expresion_Num '*' Expresion_Num             {Produ $1 $3}
    |   Expresion_Num '/' Expresion_Num             {Divi $1 $3}
    |   Expresion_Num '%' Expresion_Num             {Modu $1 $3}
    |   Expresion_Num '-' Expresion_Num             {Resta $1 $3}
    |   '-' Expresion_Num    %prec NEG              {Nega $2}
    |   Variable                                    {Var $1}
    |   Constante                                   {Int $1}

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
    |   Variable                                        {Var $1}
    |   Constante                                       {Bool $1}



{

data AST =
        Sec ListDecl Instrcs

data Instrcs = Instrcs Secuen
        | Instrcs While
        | Instrcs IfCond

data Dir = Left | Right | Up | Down

type Secuen = [InstContr]

type ListDecl = [DefRob]

data DefRob = DefRob Tipo ListIdent ListComp

data Tipo = Int | Bool | Char

type ListIdent = [Var]

type ListComp = [Comp]

data Comp = Comp Cond InstRob

data Cond = Cond Activation 
        | Cond Deactivation 
        | Cond Default 
        | Cond Expresion

data InstRob = Almac Expr
            |   Colec Var
            |   Solt Expr
            |   Mov Dir Expr
            |   ES Var
--            |   Sec_Inst -- Falta

data Var = Var {Ident::[Char], Val::[Char]}

data InstContr = ActivateInst [Var]
        |   DeactivateInst [Var]
        |   AdvanceInst [Var]

data Expr = Expr Expr_Bool | Expr Expr_Num

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
        |       Var
        |       Bool

data Expr_Num = Suma Expr_Num Expr_Num
        |       Resta Expr_Num Expr_Num
        |       Divi Expr_Num Expr_Num
        |       Produ Expr_Num Expr_Num
        |       Modu Expr_Num Expr_Num
        |       Nega Expr_Num
        |       Var
        |       Int



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
        deriving (Eq)

    -- Funcion de Error
parseError :: [Token] -> a
parseError _ = error "Error de Parser"

main = getContents >>= print . calc . alexScanTokens

}



