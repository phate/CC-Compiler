module Desugarer where

import ParserAbs

desugar :: Program -> Program
desugar (Program defs) = (Program $ cvtDefs defs)

cvtDefs :: [Def] -> [Def]
cvtDefs []            = []
cvtDefs ((CDef d):ds) = (cvtCDef d) ++ (cvtDefs ds)
cvtDefs (d:ds)        = d:(cvtDefs ds)

cvtCDef :: ClassDef -> [Def]
cvtCDef (ClassDef id decls) = strdef:fctdefs
  where strdef  = SDef $ StrDef id [ as | (CDeclA as) <- decls ]
        fctdefs = map (\f -> FDef $ cvtFct f id) [ f | (CDeclM f) <- decls ]
  
cvtFct :: FctDef -> Id -> FctDef
cvtFct (FctDef t id args (CStmt ss)) cid = (FctDef t nid nargs (CStmt $ cvtStmts ss "_this"))
  where nid =  "_" ++ cid ++ "_" ++ id
        nargs = (Arg (TIdent id) "_this"):args

cvtStmts :: [Stmt] -> Id -> [Stmt]
cvtStmts [] _      = []
cvtStmts (s:ss) id = (cvtStmt s id):(cvtStmts ss id)

cvtStmt :: Stmt -> Id -> Stmt
cvtStmt s  id = s

--cvtExpr :: Expr -> Id -> Expr
--cvtExpr (AExpr t (ESelf e)) id = (AExpr t (EPtr (EId id)  (cvtExpr e id))) 
--cvtExpr e _ = e
