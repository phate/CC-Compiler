module Desugarer where

import ParserAbs
import TypeCheckerEnv

desugar :: Program -> TCEnv -> Program
desugar (Program defs) env = (Program $ cvtDefs defs env)

cvtDefs :: [Def] -> TCEnv -> [Def]
cvtDefs [] env            = []
cvtDefs ((CDef d):ds) env = (cvtCDef d env) ++ (cvtDefs ds env)
cvtDefs ((FDef d):ds) env = (FDef (cvtFct d env)) : (cvtDefs ds env)
cvtDefs (d:ds) env        = d:(cvtDefs ds env)

cvtCDef :: ClassDef -> TCEnv -> [Def]
cvtCDef (ClassDef id decls) env = strdef:fctdefs
  where strdef  = SDef $ StrDef id [ as | (CDeclA as) <- decls ]
        fctdefs = map (\f -> FDef $ cvtMeth f id env) [ f | (CDeclM f) <- decls ]

cvtCDef (EClassDef sub base decls) env = strdef:fctdefs
  where strdef  = SDef $ StrDef sub [ as | (CDeclA as) <- decls ]
        fctdefs = map (\f -> FDef $ cvtMeth f sub env) [ f | (CDeclM f) <- decls ]

cvtFct :: FctDef -> TCEnv -> FctDef
cvtFct (FctDef t id args (CStmt ss)) env = (FctDef t id args (CStmt $ cvtStmtsFct ss env))


cvtMeth :: FctDef -> Id -> TCEnv -> FctDef
cvtMeth (FctDef t id args (CStmt ss)) cid env = (FctDef t nid nargs (CStmt $ cvtStmts ss env))
  where nid =  "_" ++ cid ++ "_" ++ id
        nargs = (Arg (TIdent cid) "_this"):args



-- WHEN CHECKING CLASS METHOD
cvtStmts :: [Stmt] -> TCEnv -> [Stmt]
cvtStmts [] env     = []
cvtStmts (s:ss) env = (cvtStmt s env):(cvtStmts ss env)

cvtStmt :: Stmt -> TCEnv -> Stmt
cvtStmt (SCStmt (CStmt ss)) env = (SCStmt (CStmt $ cvtStmts ss env))
cvtStmt (SDecl t items) env     = (SDecl t (cvtItems items env))
cvtStmt (SAss e1 e2) env        = (SAss (cvtExpr e1 env) (cvtExpr e2 env))
cvtStmt (SIncrBool x b) env     = 
  case b of
    True  -> (SAss (AExpr (DType TInt 0) (EPtr (AExpr (TIdent "") (EId "_this")) x)) 
              (AExpr (DType TInt 0) (EAdd 
                (AExpr (DType TInt 0) (EPtr (AExpr (TIdent "") (EId "_this")) x))
                Plus
                (AExpr (DType TInt 0) (EInteger 1))
                )))
    False -> (SIncr x)
cvtStmt (SDecrBool x b) env     = 
  case b of
    True  -> (SAss (AExpr (DType TInt 0) (EPtr (AExpr (TIdent "") (EId "_this")) x)) 
              (AExpr (DType TInt 0) (EAdd 
                (AExpr (DType TInt 0) (EPtr (AExpr (TIdent "") (EId "_this")) x))
                Minus
                (AExpr (DType TInt 0) (EInteger 1))
                )))
    False -> (SDecr x)
cvtStmt (SRet e) env            = (SRet (cvtExpr e env))
cvtStmt (SIf e s) env           = (SIf (cvtExpr e env) (cvtStmt s env))
cvtStmt (SIfElse e s1 s2) env   = (SIfElse (cvtExpr e env) (cvtStmt s1 env) (cvtStmt s2 env))
cvtStmt (SWhile e s) env        = (SWhile (cvtExpr e env) (cvtStmt s env))
cvtStmt (SFor t id' e s) env    = (SFor t id' (cvtExpr e env) (cvtStmt s env))
cvtStmt (SExp e) env            = (SExp (cvtExpr e env))
cvtStmt s env = s

cvtItems :: [Item] -> TCEnv -> [Item]
cvtItems [] env = []
cvtItems ((NoInit id):is) env = (NoInit id):(cvtItems is env)
cvtItems ((Init id e):is) env = (Init id (cvtExpr e env)):(cvtItems is env)


cvtExprs :: [Expr] -> TCEnv -> [Expr]
cvtExprs [] env     = []
cvtExprs (e:es) env = (cvtExpr e env):(cvtExprs es env)

cvtExpr :: Expr -> TCEnv -> Expr
cvtExpr (AExpr t (EIdBool x b)) env =
  case b of
    True  -> (AExpr t (EPtr (AExpr (TIdent "") (EId "_this")) x))
    False -> (AExpr t (EId x))

cvtExpr (AExpr t (EDot e (EId "length"))) env = (AExpr t (EDot (cvtExpr e env) (EId "length")))

cvtExpr (AExpr t (EDot e@(AExpr (TIdent cid) _) (AExpr _ (EAppTypes mid es ts)))) env = 
  (AExpr t (EAppTypes  ("_" ++ cid' ++ "_" ++ mid) ((cvtExpr e env):(cvtExprs es env)) ((TIdent cid'):ts)))
 where
   cid' = lookupClassForMethod env cid mid

cvtExpr (AExpr t ESelf) env                 = (AExpr t (EId "_this"))
cvtExpr (AExpr t (EAppTypes fun es ts)) env = (AExpr t (EAppTypes fun (cvtExprs es env) ts))
cvtExpr (AExpr t (EIndex e es)) env         = (AExpr t (EIndex (cvtExpr e env) (cvtExprs es env)))
cvtExpr (AExpr t (EPtr e field)) env        = (AExpr t (EPtr (cvtExpr e env) field))
cvtExpr (AExpr t (ENeg e)) env              = (AExpr t (ENeg (cvtExpr e env)))
cvtExpr (AExpr t (ENot e)) env              = (AExpr t (ENot (cvtExpr e env)))
cvtExpr (AExpr t (ENew t' es)) env          = (AExpr t (ENew t' (cvtExprs es env)))
cvtExpr (AExpr t (EMul e1 op e2)) env       = (AExpr t (EMul (cvtExpr e1 env) op (cvtExpr e2 env)))
cvtExpr (AExpr t (EAdd e1 op e2)) env       = (AExpr t (EAdd (cvtExpr e1 env) op (cvtExpr e2 env)))
cvtExpr (AExpr t (ERel e1 op e2)) env       = (AExpr t (ERel (cvtExpr e1 env) op (cvtExpr e2 env)))
cvtExpr (AExpr t (EAnd e1 e2)) env          = (AExpr t (EAnd (cvtExpr e1 env) (cvtExpr e2 env)))
cvtExpr (AExpr t (EOr e1 e2)) env           = (AExpr t (EOr (cvtExpr e1 env) (cvtExpr e2 env)))
cvtExpr e env = e



-- WHEN CHECKING NORMAL FUNCTION
cvtStmtsFct :: [Stmt] -> TCEnv -> [Stmt]
cvtStmtsFct [] env     = []
cvtStmtsFct (s:ss) env = (cvtStmtFct s env):(cvtStmtsFct ss env)

cvtStmtFct :: Stmt -> TCEnv -> Stmt
cvtStmtFct (SCStmt (CStmt ss)) env = (SCStmt (CStmt $ cvtStmtsFct ss env))
cvtStmtFct (SIncrBool x b) env     = (SIncr x)
cvtStmtFct (SDecrBool x b) env     = (SDecr x)
cvtStmtFct (SDecl t items) env     = (SDecl t (cvtItemsFct items env))
cvtStmtFct (SAss e1 e2) env        = (SAss (cvtExprFct e1 env) (cvtExprFct e2 env))
cvtStmtFct (SRet e) env            = (SRet (cvtExprFct e env))
cvtStmtFct (SIf e s) env           = (SIf (cvtExprFct e env) (cvtStmtFct s env))
cvtStmtFct (SIfElse e s1 s2) env   = (SIfElse (cvtExprFct e env) (cvtStmtFct s1 env) (cvtStmtFct s2 env))
cvtStmtFct (SWhile e s) env        = (SWhile (cvtExprFct e env) (cvtStmtFct s env))
cvtStmtFct (SFor t id' e s) env    = (SFor t id' (cvtExprFct e env) (cvtStmtFct s env))
cvtStmtFct (SExp e) env            = (SExp (cvtExprFct e env))
cvtStmtFct s env = s

cvtItemsFct :: [Item] -> TCEnv -> [Item]
cvtItemsFct []  env = []
cvtItemsFct ((NoInit id):is) env = (NoInit id):(cvtItemsFct is env)
cvtItemsFct ((Init id e):is) env = (Init id (cvtExprFct e env)):(cvtItemsFct is env)

cvtExprsFct :: [Expr] -> TCEnv -> [Expr]
cvtExprsFct [] env     = []
cvtExprsFct (e:es) env = (cvtExprFct e env):(cvtExprsFct es env)

cvtExprFct :: Expr -> TCEnv -> Expr
cvtExprFct (AExpr t (EDot e (EId "length"))) env = (AExpr t (EDot (cvtExprFct e env) (EId "length")))

cvtExprFct (AExpr t (EDot e@(AExpr (TIdent cid) _) (AExpr _ (EAppTypes mid es ts)))) env = 
  (AExpr t (EAppTypes  ("_" ++ cid' ++ "_" ++ mid) ((cvtExprFct e env):(cvtExprsFct es env)) ((TIdent cid'):ts)))
 where
   cid' = lookupClassForMethod env cid mid

cvtExprFct (AExpr t (EIdBool x b)) env         = (AExpr t (EId x))
cvtExprFct (AExpr t (EAppTypes fun es ts)) env = (AExpr t (EAppTypes fun (cvtExprsFct es env) ts))
cvtExprFct (AExpr t (EIndex e es)) env         = (AExpr t (EIndex (cvtExprFct e env) (cvtExprsFct es env)))
cvtExprFct (AExpr t (EPtr e field)) env        = (AExpr t (EPtr (cvtExprFct e env) field))
cvtExprFct (AExpr t (ENeg e)) env              = (AExpr t (ENeg (cvtExprFct e env)))
cvtExprFct (AExpr t (ENot e)) env              = (AExpr t (ENot (cvtExprFct e env)))
cvtExprFct (AExpr t (ENew t' es)) env          = (AExpr t (ENew t' (cvtExprsFct es env)))
cvtExprFct (AExpr t (EMul e1 op e2)) env       = (AExpr t (EMul (cvtExprFct e1 env) op (cvtExprFct e2 env)))
cvtExprFct (AExpr t (EAdd e1 op e2)) env       = (AExpr t (EAdd (cvtExprFct e1 env) op (cvtExprFct e2 env)))
cvtExprFct (AExpr t (ERel e1 op e2)) env       = (AExpr t (ERel (cvtExprFct e1 env) op (cvtExprFct e2 env)))
cvtExprFct (AExpr t (EAnd e1 e2)) env          = (AExpr t (EAnd (cvtExprFct e1 env) (cvtExprFct e2 env)))
cvtExprFct (AExpr t (EOr e1 e2)) env           = (AExpr t (EOr (cvtExprFct e1 env) (cvtExprFct e2 env)))
cvtExprFct e env = e

