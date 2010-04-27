Usage:
Invoke the compiler with: jlc <source_file>.
...

Features implemented:
...

Feature NOT implemented:
...

Javalette Specification:
The grammar used for parsing the Javalette language is given below.
Words beginning with a capital letter denote nonterminals and the
rest denotes terminales, whereas words enclosed in single quotation
marks are matched literally and the other ones, like id, double, etc.
according to their definition in the project description.
This grammar produces one shift/reduce conflict (dangling else). However,
since the parser happy always chooses a shift over an reduce, this is no
problem.

Program     : ListFctDef
ListFctDef  : FctDef
            | FctDef ListFctDef
FctDef      : Type id '(' ListArg ')' CmpStmt
ListArg     : {- empty -}
            | Arg
            | Arg ',' ListArg
Arg         : Type id
CmpStmt     : '{' ListStmt '}'
ListStmt    : {- empty -}
            | ListStmt Stmt
Stmt        : ';'
            | CmpStmt
            | Type ListItem ';'
            | id '=' Expr ';'
            | id '++' ';'
            | id '--' ';'
            | 'return' Expr ';'
            | 'return' ';'
            | 'if' '(' Expr ')' Stmt
            | 'if' '(' Expr ')' Stmt 'else' Stmt
            | 'while' '(' Expr ')' Stmt
            | Expr ';'
ListItem    : Item
            | Item ',' ListItem
Item        : id
            | id '=' Expr
Type        : 'int'
            | 'double'
            | 'boolean'
            | 'void'
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
Expr5       : '!' Expr6
            | '-' Expr6
            | Expr6
Expr6       : id '(' string ')'
            | id '(' ListExpr ')'
            | 'false'
            | 'true'
            | double
            | integer
            | id
            | '(' Expr ')'
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
