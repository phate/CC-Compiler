module ParserAbs where

type Id = String

data Program =
  Program [FctDef]
  deriving Show

data FctDef =
  FctDef Type Id [Arg] CmpStmt
  deriving Show

data Arg =
  Arg Type Id
  deriving Show

data CmpStmt =
  CStmt [Stmt]
  deriving Show

data Stmt =
    SEmpty
  | SCStmt CmpStmt
  | SDecl Type [Item]
  | SAss Id Expr
  | SIncr Id
  | SDecr Id
  | SRet Expr
  | SVRet
  | SIf Expr Stmt
  | SIfElse Expr Stmt Stmt
  | SWhile Expr Stmt
  | SExp Expr
  deriving Show

data Item =
    NoInit Id
  | Init Id Expr
  deriving Show

data Type =
    TInt
  | TDouble
  | TBool
  | TVoid
	| TString
  deriving (Eq, Show)

data Expr =
    EId Id
  | EInteger Integer
  | EDouble Double
  | ETrue
  | EFalse
  | EApp Id [Expr]
  | EAppS Id String
  | ENeg Expr
  | ENot Expr
  | EMul Expr MulOp Expr
  | EAdd Expr AddOp Expr
  | ERel Expr RelOp Expr
  | EAnd Expr Expr
  | EOr Expr Expr
  | AExpr Type Expr
  deriving Show

data AddOp =
    Plus
  | Minus
  deriving Show

data MulOp =
    Mul
  | Div
  | Mod
  deriving Show

data RelOp =
    Lth
  | Leq
  | Gth
  | Geq
  | Eq
  | Neq
  deriving Show
