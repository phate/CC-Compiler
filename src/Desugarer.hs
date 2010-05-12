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
  where strdef  = SDef $ StrDef id (concat [ as | (CDeclA as) <- decls ])
        fctdefs = map (\f -> FDef $ cvtFct f id) [ f | (CDeclM f) <- decls ]
  
cvtFct :: FctDef -> Id -> FctDef
cvtFct (FctDef t id args ss) cid = (FctDef t nid nargs ss)
  where nid =  "_" ++ cid ++ "_" ++ id
        nargs = (Arg (TIdent id) "_this"):args

cvtStmts :: [Stmt] -> [Stmt]
cvtStmts []     = []
cvtStmts (s:ss) = (cvtStmt s):(cvtStmts ss)

cvtStmt :: Stmt -> Stmt
cvtStmt s = s
