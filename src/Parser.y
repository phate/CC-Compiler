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
	'if'			{ TIf }
	'else'		{ TElse }
	'while'		{ TWhile }
	'int'			{ TTInt }
	'double'	{ TTDouble }
	'boolean'	{ TTBool }
	'void'		{ TTVoid }
	'true'		{ TTrue }
	'false'		{ TFalse }
	'('				{ TOB }
	')'				{ TCB }
	'{'				{ TOCB }
	'}'				{ TCCB }
	';'				{ TSemi }
	','				{ TComma }
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

%%

Program 		:	ListFctDef													{	Program $1 }
ListFctDef 	: FctDef															{ (:[]) $1 }
						| FctDef ListFctDef										{ (:) $1 $2 }
FctDef			: Type id '(' ListArg ')' CmpStmt			{ FctDef $1 $2 $4 $6 }
ListArg			: {- empty -}													{ [] }
						| Arg																	{ (:[]) $1 }
						| Arg ',' ListArg 										{ (:) $1 $3 }
Arg					: Type id															{ Arg $1 $2 }
CmpStmt			: '{' ListStmt '}'										{ CStmt (reverse $2) }
ListStmt		: {- empty -}													{ [] } 
						| ListStmt Stmt												{ flip (:) $1 $2 }
Stmt				: ';'																	{ SEmpty }
						| CmpStmt 														{ SCStmt $1 }
						| Type ListItem ';'										{ SDecl $1 $2 }
						| id '=' Expr ';'											{ SAss $1 $3 }
						| id '++' ';'													{ SIncr $1 }
						| id '--' ';'													{ SDecr $1 }
						| 'return' Expr	';'										{ SRet $2 }
						| 'return' ';'												{ SVRet }
						| 'if' '(' Expr ')' Stmt							{ SIf $3 $5 }
						| 'if' '(' Expr ')' Stmt 'else' Stmt	{ SIfElse $3 $5 $7 }
						| 'while' '(' Expr ')' Stmt						{ SWhile $3 $5 }
						| Expr ';'														{ SExp $1 }
ListItem		: Item																{ (:[]) $1 }
						| Item ',' ListItem 									{ (:) $1 $3 }
Item				: id																	{ NoInit $1 }
						| id '=' Expr													{ Init $1 $3 }
Type				: 'int'																{ TInt }
						|	'double'														{ TDouble }
						| 'boolean'														{ TBool }
						| 'void'															{ TVoid }
Expr				: Expr1 '||' Expr											{ EOr $1 $3 }
						|	Expr1																{ $1 }
Expr1				: Expr2 '&&' Expr1										{ EAnd $1 $3 }
						| Expr2																{ $1 }
Expr2				: Expr2 RelOp Expr3										{ ERel $1 $2 $3 }
						| Expr3																{ $1 }
Expr3				: Expr3 AddOp Expr4										{ EAdd $1 $2 $3 }
						| Expr4																{ $1 }
Expr4				: Expr4 MulOp	Expr5										{ EMul $1 $2 $3 }
						| Expr5																{ $1 }
Expr5				: '!' Expr6														{ ENot $2 }
						| '-' Expr6														{ ENeg $2 }
						| Expr6																{ $1 }
Expr6				: id '(' string ')'										{ EAppS $1 $3 }
						| id '(' ListExpr ')'									{ EApp $1 $3 }
						| 'false'															{ EFalse }
						| 'true'															{ ETrue }
						| double															{ EDouble $1 }
						| integer															{ EInteger $1 }
						| id																	{ EId $1 }
						| '(' Expr ')'												{ $2 }
ListExpr		:	{- empty -}													{ [] }
						| Expr																{ (:[]) $1 }
						| Expr ',' ListExpr										{ (:) $1 $3 }
MulOp				: '*'																	{ Mul }
						| '/'																	{ Div }
						| '%'																	{ Mod }
AddOp				: '+'																	{ Plus }
						| '-'																	{ Minus }
RelOp				: '<'																	{ Lth }	
						| '<='																{ Leq }
						| '>'																	{ Gth }
						| '>='																{ Geq }
						| '=='																{ Eq }
						| '!='																{ Neq }															
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
	= TId	String
	| TPInt Integer
	| TPDouble Double
	| TPString	String
	| TReturn
	|	TIf
	| TElse
	| TWhile
	| TTInt
	| TTDouble
	| TTBool
	| TTVoid
	| TTrue
	| TFalse
	| TOB
	| TCB
	| TOCB
	| TCCB
	| TSemi
	| TComma
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
	deriving Show
}
