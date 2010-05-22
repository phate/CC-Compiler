module Desugarer where

import ParserAbs

desugar :: Program -> Program
desugar (Program defs) = (Program $ cvtDefs defs)

cvtDefs :: [Def] -> [Def]
cvtDefs []            = []
cvtDefs ((CDef d):ds) = (cvtCDef d) ++ (cvtDefs ds)
cvtDefs ((FDef d):ds) = (FDef (cvtFct d)) : (cvtDefs ds)
cvtDefs (d:ds)        = d:(cvtDefs ds)

cvtCDef :: ClassDef -> [Def]
cvtCDef (ClassDef id decls) = strdef:fctdefs
  where strdef  = SDef $ StrDef id [ as | (CDeclA as) <- decls ]
        fctdefs = map (\f -> FDef $ cvtMeth f id) [ f | (CDeclM f) <- decls ]

cvtCDef (EClassDef sub base decls) = strdef:fctdefs
  where strdef  = SDef $ StrDef sub [ as | (CDeclA as) <- decls ]
        fctdefs = map (\f -> FDef $ cvtMeth f sub) [ f | (CDeclM f) <- decls ]

cvtFct :: FctDef -> FctDef
cvtFct (FctDef t id args (CStmt ss)) = (FctDef t id args (CStmt $ cvtStmtsFct ss))


cvtMeth :: FctDef -> Id -> FctDef
cvtMeth (FctDef t id args (CStmt ss)) cid = (FctDef t nid nargs (CStmt $ cvtStmts ss))
  where nid =  "_" ++ cid ++ "_" ++ id
        nargs = (Arg (TIdent cid) "_this"):args



-- WHEN CHECKING NORMAL FUNCTION
cvtStmtsFct :: [Stmt] -> [Stmt]
cvtStmtsFct []     = []
cvtStmtsFct (s:ss) = (cvtStmtFct s):(cvtStmtsFct ss)

cvtStmtFct :: Stmt -> Stmt
cvtStmtFct (SCStmt (CStmt ss)) = (SCStmt (CStmt $ cvtStmtsFct ss))
cvtStmtFct (SDecl t items)     = (SDecl t items) -- TODO THIS ONE!!!
cvtStmtFct (SAss e1 e2)        = (SAss (cvtExprFct e1) (cvtExprFct e2))
cvtStmtFct (SRet e)            = (SRet (cvtExprFct e))
cvtStmtFct (SIf e s)           = (SIf (cvtExprFct e) (cvtStmtFct s))
cvtStmtFct (SIfElse e s1 s2)   = (SIfElse (cvtExprFct e) (cvtStmtFct s1) (cvtStmtFct s2))
cvtStmtFct (SWhile e s)        = (SWhile (cvtExprFct e) (cvtStmtFct s))
cvtStmtFct (SFor t id' e s)    = (SFor t id' (cvtExprFct e) (cvtStmtFct s))
cvtStmtFct (SExp e)            = (SExp (cvtExprFct e))
cvtStmtFct s = s


-- WHEN CHECKING CLASS METHOD
cvtStmts :: [Stmt] -> [Stmt]
cvtStmts []     = []
cvtStmts (s:ss) = (cvtStmt s):(cvtStmts ss)
{-
cvtStmts :: [Stmt] -> Id -> [Stmt]
cvtStmts [] _      = []
cvtStmts (s:ss) id = (cvtStmt s id):(cvtStmts ss id)
-}

cvtStmt :: Stmt -> Stmt
cvtStmt (SCStmt (CStmt ss)) = (SCStmt (CStmt $ cvtStmts ss))
cvtStmt (SDecl t items)     = (SDecl t items) -- TODO THIS ONE!!!
cvtStmt (SAss e1 e2)        = (SAss (cvtExpr e1) (cvtExpr e2))
cvtStmt (SIncr x)           = (SIncr x) -- TODO THIS ONE!!!
cvtStmt (SDecr x)           = (SDecr x) -- TODO THIS ONE!!!
cvtStmt (SRet e)            = (SRet (cvtExpr e))
cvtStmt (SIf e s)           = (SIf (cvtExpr e) (cvtStmt s))
cvtStmt (SIfElse e s1 s2)   = (SIfElse (cvtExpr e) (cvtStmt s1) (cvtStmt s2))
cvtStmt (SWhile e s)        = (SWhile (cvtExpr e) (cvtStmt s))
cvtStmt (SFor t id' e s)    = (SFor t id' (cvtExpr e) (cvtStmt s))
cvtStmt (SExp e)            = (SExp (cvtExpr e))
cvtStmt s = s
{-
cvtStmt :: Stmt -> Id -> Stmt
cvtStmt (SCStmt (CStmt ss)) id = (SCStmt (CStmt $ cvtStmts ss id))
cvtStmt (SAss e1 e2) id        = (SAss (cvtExpr e1 id) (cvtExpr e2 id))
cvtStmt (SRet e) id            = (SRet (cvtExpr e id))
cvtStmt (SIf e s) id           = (SIf (cvtExpr e id) (cvtStmt s id))
cvtStmt (SIfElse e s1 s2) id   = (SIfElse (cvtExpr e id) (cvtStmt s1 id) (cvtStmt s2 id))
cvtStmt (SWhile e s) id        = (SWhile (cvtExpr e id) (cvtStmt s id))
cvtStmt (SFor t id' e s) id    = (SFor t id' (cvtExpr e id) (cvtStmt s id))
cvtStmt (SExp e) id            = (SExp (cvtExpr e id))
cvtStmt s  id = s
-}
{-
-}


-- WHEN CHECKING CLASS METHOD
cvtExprs :: [Expr] -> [Expr]
cvtExprs []     = []
cvtExprs (e:es) = (cvtExpr e):(cvtExprs es)

cvtExpr :: Expr -> Expr
cvtExpr (AExpr t (EId x)) = (AExpr t (EPtr (AExpr (TIdent "Node") (EId "_this")) x)) --Need to check env here whether its instance variable or not...
cvtExpr (AExpr t (EDot e@(AExpr (TIdent cid) _) (AExpr _ (EApp mid es)))) = (AExpr t (EApp ("_" ++ cid ++ "_" ++ mid) ((cvtExpr e):(cvtExprs es))))
cvtExpr (AExpr t ESelf) = (AExpr t (EId "_this"))


cvtExpr (AExpr t (EApp fun es))   = (AExpr t (EApp fun (cvtExprs es)))
cvtExpr (AExpr t (EIndex e es))   = (AExpr t (EIndex (cvtExpr e) (cvtExprs es)))
cvtExpr (AExpr t (EPtr e field))  = (AExpr t (EPtr (cvtExpr e) field))
cvtExpr (AExpr t (ENeg e))        = (AExpr t (ENeg (cvtExpr e)))
cvtExpr (AExpr t (ENot e))        = (AExpr t (ENot (cvtExpr e)))
cvtExpr (AExpr t (ENew t' es))    = (AExpr t (ENew t' (cvtExprs es)))
cvtExpr (AExpr t (EMul e1 op e2)) = (AExpr t (EMul (cvtExpr e1) op (cvtExpr e2)))
cvtExpr (AExpr t (EAdd e1 op e2)) = (AExpr t (EAdd (cvtExpr e1) op (cvtExpr e2)))
cvtExpr (AExpr t (ERel e1 op e2)) = (AExpr t (ERel (cvtExpr e1) op (cvtExpr e2)))
cvtExpr (AExpr t (EAnd e1 e2))    = (AExpr t (EAnd (cvtExpr e1) (cvtExpr e2)))
cvtExpr (AExpr t (EOr e1 e2))     = (AExpr t (EOr (cvtExpr e1) (cvtExpr e2)))
cvtExpr e = e



-- WHEN CHECKING NORMAL FUNCTION
cvtExprsFct :: [Expr] -> [Expr]
cvtExprsFct []     = []
cvtExprsFct (e:es) = (cvtExprFct e):(cvtExprsFct es)

cvtExprFct :: Expr -> Expr
cvtExprFct (AExpr t (EDot e@(AExpr (TIdent cid) _) (AExpr _ (EApp mid es)))) = (AExpr t (EApp ("_" ++ cid ++ "_" ++ mid) ((cvtExprFct e):(cvtExprsFct es))))

cvtExprFct (AExpr t (EApp fun es))   = (AExpr t (EApp fun (cvtExprsFct es)))
cvtExprFct (AExpr t (EIndex e es))   = (AExpr t (EIndex (cvtExprFct e) (cvtExprsFct es)))
cvtExprFct (AExpr t (EPtr e field))  = (AExpr t (EPtr (cvtExprFct e) field))
cvtExprFct (AExpr t (ENeg e))        = (AExpr t (ENeg (cvtExprFct e)))
cvtExprFct (AExpr t (ENot e))        = (AExpr t (ENot (cvtExprFct e)))
cvtExprFct (AExpr t (ENew t' es))    = (AExpr t (ENew t' (cvtExprsFct es)))
cvtExprFct (AExpr t (EMul e1 op e2)) = (AExpr t (EMul (cvtExprFct e1) op (cvtExprFct e2)))
cvtExprFct (AExpr t (EAdd e1 op e2)) = (AExpr t (EAdd (cvtExprFct e1) op (cvtExprFct e2)))
cvtExprFct (AExpr t (ERel e1 op e2)) = (AExpr t (ERel (cvtExprFct e1) op (cvtExprFct e2)))
cvtExprFct (AExpr t (EAnd e1 e2))    = (AExpr t (EAnd (cvtExprFct e1) (cvtExprFct e2)))
cvtExprFct (AExpr t (EOr e1 e2))     = (AExpr t (EOr (cvtExprFct e1) (cvtExprFct e2)))
cvtExprFct e = e

{-
cvtExprs :: [Expr] -> Id -> [Expr]
cvtExprs [] _      = []
cvtExprs (e:es) id = (cvtExpr e id):(cvtExprs es id)

cvtExpr :: Expr -> Id -> Expr
cvtExpr (AExpr t (EId x)) id = (AExpr t (EPtr (AExpr (TIdent id) (EId "_this")) x)) --Need to check env here whether its instance variable or not...
cvtExpr (AExpr t (EDot e (AExpr _ (EApp mid es)))) id = (AExpr t (EApp ("_" ++ id ++ "_" ++ mid) ((cvtExpr e id):(cvtExprs es id))))
cvtExpr (AExpr t ESelf) id = (AExpr t (EId "_this"))

cvtExpr (AExpr t (EApp fun es)) id   = (AExpr t (EApp fun (cvtExprs es id)))
cvtExpr (AExpr t (EIndex e es)) id   = (AExpr t (EIndex (cvtExpr e id) (cvtExprs es id)))
cvtExpr (AExpr t (EPtr e field)) id  = (AExpr t (EPtr (cvtExpr e id) field))
cvtExpr (AExpr t (ENeg e)) id        = (AExpr t (ENeg (cvtExpr e id)))
cvtExpr (AExpr t (ENot e)) id        = (AExpr t (ENot (cvtExpr e id)))
cvtExpr (AExpr t (ENew t' es)) id    = (AExpr t (ENew t' (cvtExprs es id)))
cvtExpr (AExpr t (EMul e1 op e2)) id = (AExpr t (EMul (cvtExpr e1 id) op (cvtExpr e2 id)))
cvtExpr (AExpr t (EAdd e1 op e2)) id = (AExpr t (EAdd (cvtExpr e1 id) op (cvtExpr e2 id)))
cvtExpr (AExpr t (ERel e1 op e2)) id = (AExpr t (ERel (cvtExpr e1 id) op (cvtExpr e2 id)))
cvtExpr (AExpr t (EAnd e1 e2)) id    = (AExpr t (EAnd (cvtExpr e1 id) (cvtExpr e2 id)))
cvtExpr (AExpr t (EOr e1 e2)) id     = (AExpr t (EOr (cvtExpr e1 id) (cvtExpr e2 id)))
cvtExpr e _ = e
-}

