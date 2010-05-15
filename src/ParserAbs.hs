module ParserAbs where

type Id = String

data Program =
  Program [Def]
  deriving Show

data Def =
    FDef FctDef
  | SDef StrDef
  | TDef TypeDef
  | CDef ClassDef
  deriving Show

data ClassDef =
    ClassDef Id [CDecl]
  | EClassDef Id Id [CDecl]
  deriving Show

data CDecl =
    CDeclM FctDef
  | CDeclA (DType,Id)
  deriving Show

data TypeDef =
  TypeDef Id Id
  deriving Show

data StrDef =
  StrDef Id [(DType,Id)]
  deriving Show

data FctDef =
  FctDef DType Id [Arg] CmpStmt
  deriving Show

data Arg =
  Arg DType Id
  deriving Show

data CmpStmt =
  CStmt [Stmt]
  deriving Show

data Stmt =
    SEmpty
  | SCStmt CmpStmt
  | SDecl DType [Item]
  | SAss Id [Expr] Expr
  | SIncr Id
  | SDecr Id
  | SRet Expr
  | SVRet
  | SIf Expr Stmt
  | SIfElse Expr Stmt Stmt
  | SWhile Expr Stmt
  | SExp Expr
  | SDerf Expr Id Expr
  deriving Show

data Item =
    NoInit Id
  | Init Id Expr
  deriving Show

data DType =
    DType Type Int
  | TIdent Id
  | TVoid
  | TString
  | TPtr8
  | TArr Type
  deriving (Eq, Show)

data Type =
    TInt
  | TDouble
  | TBool
--  | TVoid
--	| TString
--  | TStringP
  deriving (Eq, Show)

data Expr =
    EId Id
  | EInteger Integer
  | EDouble Double
  | ETrue
  | EFalse
  | EApp Id [Expr]
  | EAppS Id String
  | EIdx Id [Expr]
  | EDot Expr Expr
  | EPtr Expr Id
  | ESelf
  | ENull Id
  | ENeg Expr
  | ENot Expr
  | ENew DType [Expr]
  | EMul Expr MulOp Expr
  | EAdd Expr AddOp Expr
  | ERel Expr RelOp Expr
  | EAnd Expr Expr
  | EOr Expr Expr
  | AExpr DType Expr
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
