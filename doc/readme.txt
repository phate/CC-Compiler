Usage:
Invoke the compiler with: jlc <source_file>.
The output of the compiler is a file containing LLVM assembly language
for the programme contained in <source_file>.
Also outputted is a.out which, when executed, executes a.out.bc (containing LLVM bitcode).

Javalette Specification:
The grammar used for parsing the Javalette language is given below.
Words beginning with a capital letter denote nonterminals and the
rest denotes terminales, whereas words enclosed in single quotation
marks are matched literally and the other ones, like id, double, etc.
according to their definition in the project description.

Program     : ListDef
ListDef     : Def
            | Def ListDef
Def         : FctDef
            | StrDef
            | TypeDef
            | ClassDef
ClassDef    : 'class' id '{' ListCDecl '}'
            | 'class' id 'extends' id '{' ListCDecl '}'
ListCDecl   : {- empty -}
            | FctDef ListCDecl
            | TId ';' ListCDecl
TypeDef     : 'typedef' 'struct' id '*' id ';'
StrDef      : 'struct' id '{' ListTId '}' ';'
ListTId     : {- empty -}
            | TId ';' ListTId
FctDef      : TId '(' LListArg ')' CmpStmt
LListArg    : {- empty -}
            | ListArg
ListArg     : TId
            | TId ',' ListArg
CmpStmt     : '{' ListStmt '}'
ListStmt    : {- empty -}
            | ListStmt Stmt
Stmt        : ';'
            | CmpStmt
            | Type ListItem ';'
            | Expr '=' Expr ';'
            | id '++' ';'
            | id '--' ';'
            | 'return' Expr ';'
            | 'return' ';'
            | 'for' '(' Type id ':' Expr ')' Stmt
            | 'if' '(' Expr ')' Stmt
            | 'if' '(' Expr ')' Stmt 'else' Stmt
            | 'while' '(' Expr ')' Stmt
            | Expr ';'
ListItem    : Item
            | Item ',' ListItem
Item        : id
            | id '=' Expr
TId         : 'int' ListSB id
            | 'double' ListSB id
            | 'boolean' ListSB id
            | 'void' id
            | id id
Type        : 'int' ListSB
            | 'double' ListSB
            | 'boolean' ListSB
            | 'void'
            | id
ListSB      : {- empty -}
            | '['']' ListSB
Expr        : Expr1 '||' Expr
            | Expr1
Expr1       : Expr2 '&&' Expr1
            | Expr2
Expr2       : Expr2 RelOp Expr3
            | Expr3
Expr3       : Expr3 AddOp Expr4
            | Expr4
Expr4       : Expr4 MulOp Expr5
            | Expr5
Expr5       : 'new' 'int' ListIdx
            | 'new' 'double' ListIdx
            | 'new' 'boolean' ListIdx
            | 'new' id
            | Expr6
Expr6       : '!' Expr7
            | '-' Expr7
            | Expr7
Expr7       : Expr7 '.' Expr8
            | Expr8 ListIdx
            | Expr8
Expr8       : 'self'
            | '(' id ')' 'null'
            | Expr8 '->' id
            | id '(' string ')'
            | id '(' ListExpr ')'
            | 'false'
            | 'true'
            | double
            | integer
            | id
            | '(' Expr ')'
ListIdx     : '[' Expr ']' ListIdx
            | '[' Expr ']'
ListExpr    : {- empty -}
            | Expr
            | Expr ',' ListExpr
MulOp       : '*'
            | '/'
            | '%'
AddOp       : '+'
            | '-'
RelOp       : '<'
            | '<='
            | '>'
            | '>='
            | '=='
            | '!='



Shift/reduce conflicts:
This grammar produces 2 shift/reduce conflicts:
  * Dangling else: Since the parser happy always chooses a shift over an reduce, this is no problem.
  * ( Ident ) null vs ( Exp ): After the parser shifted 'null' on the stack, it will know which rule
                                it has choose for reduction.

Features implemented:
The following extensions have been implemented:
  * arrays1 (6.1 One-dimensional arrays and for loops)
  * arrays2 (6.2 Multidimensional arrays)
  * pointers (6.3 Dynamic data structures)
  * objects1 (6.4 Object-orientation)

