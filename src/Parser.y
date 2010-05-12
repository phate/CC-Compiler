{
module Parser where
import ParserAbs
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	id				{ TId $$ }
	integer		{ TPInt $$ }
	double		{ TPDouble $$ }
	string		{ TPString $$ }
	'return' 	{ TReturn }
  'struct'  { TStruct }
  'typedef' { TTypeDef }
  'class'   { TClass }
  'extends' { TExtends }
  'self'    { TSelf }
  'null'    { TNull }
  'if'			{ TIf }
	'else'		{ TElse }
	'while'		{ TWhile }
	'int'			{ TTInt }
	'double'	{ TTDouble }
	'boolean'	{ TTBool }
	'void'		{ TTVoid }
	'true'		{ TTrue }
	'false'		{ TFalse }
	'new'     { TNew }
  '('				{ TOB }
	')'				{ TCB }
  '['       { TOSB }
  ']'       { TCSB }
	'{'				{ TOCB }
	'}'				{ TCCB }
	';'				{ TSemi }
	','				{ TComma }
	'.'       { TDot }
  '++'			{ TIncr }
	'--'			{ TDecr }
	'='				{ TAss }
	'||'			{ TOr }
	'&&'			{ TAnd }
	'!'				{ TNot }
	'-'				{ TMinus }
	'+'				{ TPlus }
	'*'				{ TTimes }
	'/'				{ TDiv }
	'%'				{ TMod }
	'<'				{ TLth }
	'<='			{ TLeq }
	'>'				{	TGth }
	'>='			{ TGeq }
	'=='			{ TEq }
	'!='			{ TNeq }
  '->'      { TDerf }

%%

Program 		:	ListDef													              {	Program $1 }
ListDef     : Def                                           { (:[]) $1 }
            | Def ListDef                                   { (:) $1 $2 }
Def         : FctDef                                        { FDef $1 }
            | StrDef                                        { SDef $1 }
            | TypeDef                                       { TDef $1 }
            | ClassDef                                      { CDef $1 }
ClassDef    : 'class' id '{' ListCDecl '}' ';'              { ClassDef $2 $4 }
            | 'class' id 'extends' id '{' ListCDecl '}' ';' { EClassDef $2 $4 $6 }
ListCDecl   : FctDef                                        { (:[]) (CDeclM $1) }
            | ListStrDecl ';'                               { (:[]) (CDeclA $1) }
            | FctDef ListCDecl                              { (:) (CDeclM $1) $2 }
            | ListStrDecl ';' ListCDecl                     { (:) (CDeclA $1) $3 }
TypeDef     : 'typedef' 'struct' id '*' id ';'              { TypeDef $3 $5 }
StrDef      : 'struct' id '{' ListStrDecl ';' '}' ';'       { StrDef $2 $4 }
ListStrDecl : Type ListId                                   { map (\id -> ($1,id)) $2 }
            | Type ListId ';' ListStrDecl                   { (map (\id -> ($1,id)) $2) ++ $4 }
ListId      : id                                            { (:[]) $1 }
            | id ',' ListId                                 { (:) $1 $3 }
FctDef			: Type id '(' ListArg ')' CmpStmt		            { FctDef $1 $2 $4 $6 }
ListArg			: {- empty -}													          { [] }
						| Arg																	          { (:[]) $1 }
						| Arg ',' ListArg 										          { (:) $1 $3 }
Arg					: Type id															          { Arg $1 $2 }
CmpStmt			: '{' ListStmt '}'										          { CStmt (reverse $2) }
ListStmt		: {- empty -}													          { [] } 
						| ListStmt Stmt												          { flip (:) $1 $2 }
Stmt				: ';'																	          { SEmpty }
						| CmpStmt 														          { SCStmt $1 }
						| Type ListItem ';'										          { SDecl $1 $2 }
						| id ListIdx '=' Expr ';'							          { SAss $1 $2 $4 }
						| id '++' ';'													          { SIncr $1 }
						| id '--' ';'													          { SDecr $1 }
						| 'return' Expr	';'										          { SRet $2 }
						| 'return' ';'												          { SVRet }
						| 'if' '(' Expr ')' Stmt							          { SIf $3 $5 }
						| 'if' '(' Expr ')' Stmt 'else' Stmt	          { SIfElse $3 $5 $7 }
						| 'while' '(' Expr ')' Stmt						          { SWhile $3 $5 }
						| Expr ';'														          { SExp $1 }
            | id '->' id '=' Expr ';'                       { SDerf $1 $3 $5 }
ListItem		: Item																          { (:[]) $1 }
						| Item ',' ListItem 									          { (:) $1 $3 }
Item				: id																	          { NoInit $1 }
						| id '=' Expr													          { Init $1 $3 }
Type				: 'int' ListSB												          { DType TInt $2 }
						|	'double' ListSB											          { DType TDouble $2 }
						| 'boolean' ListSB										          { DType TBool $2 }
						| 'void'															          { TVoid }
PType       : 'int'                                         { DType TInt 0 }
            | 'double'                                      { DType TDouble 0 }
            | 'boolean'                                     { DType TBool 0 }
            | id                                            { TIdent $1 }
ListSB      : {- empty -}                                   { 0 }
            | '['']' ListSB                                 { (+1) $3 }
Expr				: Expr1 '||' Expr										  	        { EOr $1 $3 }
						|	Expr1																          { $1 }
Expr1				: Expr2 '&&' Expr1										          { EAnd $1 $3 }
						| Expr2																          { $1 }
Expr2				: Expr2 RelOp Expr3										          { ERel $1 $2 $3 }
						| Expr3																          { $1 }
Expr3				: Expr3 AddOp Expr4										          { EAdd $1 $2 $3 }
						| Expr4																          { $1 }
Expr4				: Expr4 MulOp	Expr5										          { EMul $1 $2 $3 }
						| Expr5																          { $1 }
Expr5       : 'new' PType ListIdx                           { ENew $2 $3 }                         
            | Expr6                                         { $1 }
Expr6				: '!' Expr7														          { ENot $2 }
						| '-' Expr7														          { ENeg $2 }
						| Expr7																          { $1 }
Expr7       : id '.' id                                     { EDot $1 $3 }
            | 'self' '.' id                                 { ESelf $3 }
            | id '->' id                                    { EPtr $1 $3 }
            | Expr8                                         { $1 }
Expr8				: id '(' string ')'										          { EAppS $1 (take ((length $3) - 2) (drop 1 $3))}
						| id '(' ListExpr ')'									          { EApp $1 $3 }
						| 'false'															          { EFalse }
						| 'true'															          { ETrue }
						| double															          { EDouble $1 }
						| integer															          { EInteger $1 }
						| id ListIdx													          { if null $2 then EId $1 else EIdx $1 $2 }
            | '(' id ')' 'null'                             { ENull $2 }
						| '(' Expr ')'												          { $2 }
ListIdx     : {- empty -}                                   { [] }
            | '[' Expr ']' ListIdx                          { (:) $2 $4 }
ListExpr		:	{- empty -}													          { [] }
						| Expr																          { (:[]) $1 }
						| Expr ',' ListExpr										          { (:) $1 $3 }
MulOp				: '*'																	          { Mul }
						| '/'																	          { Div }
						| '%'																	          { Mod }
AddOp				: '+'																	          { Plus }
						| '-'																	          { Minus }
RelOp				: '<'																	          { Lth }	
						| '<='																          { Leq }
						| '>'																	          { Gth }
						| '>='																          { Geq }
						| '=='																          { Eq }
						| '!='																          { Neq }															
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
	= TId	String
	| TPInt Integer
	| TPDouble Double
	| TPString	String
	| TReturn
	| TStruct
  | TTypeDef 
  | TClass
  | TExtends
  | TSelf
  | TNull 
  |	TIf
	| TElse
	| TWhile
	| TTInt
	| TTDouble
	| TTBool
	| TTVoid
	| TTrue
	| TFalse
	| TNew 
  | TOB
	| TCB
	| TOSB
  | TCSB
  | TOCB
	| TCCB
	| TSemi
	| TComma
	| TDot
  | TIncr
	| TDecr
	| TAss
	| TOr
	| TAnd
	| TNot
	| TMinus
	| TPlus
	| TTimes
	| TDiv
	| TMod
	| TLth
	| TLeq
	| TGth
	| TGeq
	| TEq
	| TNeq
  | TDerf	
	deriving Show
}
