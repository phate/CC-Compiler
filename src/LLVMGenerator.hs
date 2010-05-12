module LLVMGenerator where

import ParserAbs
import LLVMEnv
import LLVMAbs
import Control.Monad.State

generateInstructions :: Program -> LLVMEnv
generateInstructions (Program defs) = execState (genDefs defs) newLLVMEnv

genDefs :: [Def] -> S()
genDefs []            = return ()
genDefs ((FDef f):ds) = genFctDef f

genFctDef :: FctDef -> S()
genFctDef (FctDef t id args (CStmt ss)) = do  let ps = [ (t,i) | Arg t i <- args]
                                              newFct t id ps
                                              addParams ps
                                              mapM_ genParams ps
                                              genStmts ss
                                              linstr <- getLastInstr
                                              case linstr of
                                                (LLLabel "lab0")  -> addInstr LLVReturn
                                                (LLLabel _)       -> addInstr Unreachable
                                                _                 -> return ()
                                          where
                                              genParams (t,i) = do  (lid,_) <- lookupVar i
                                                                    let plid = createPLLVMId lid
                                                                    addInstr (LLAlloc (OId lid) t)
                                                                    addInstr (LLStore t (OId plid) (OId lid))

genStmts :: [Stmt] -> S()
genStmts = mapM_ genStmt

genStmt :: Stmt -> S()
genStmt SEmpty              = return ()
genStmt (SCStmt (CStmt ss)) = do  pushScope
                                  genStmts ss
                                  popScope
genStmt (SRet e)            = do  (t,v) <- genExp e
                                  addInstr (LLReturn t v)
genStmt SVRet               = do  addInstr LLVReturn
genStmt (SAss x _ e)        = do  (lid,t) <- lookupVar x
                                  (_,v) <- genExp e
                                  addInstr (LLStore t v (OId lid))
genStmt (SDecl t vs) = do   mapM_ genDecl vs
                            where
                              genDecl (NoInit x) = do lid <- addVar x t
                                                      addInstr (LLAlloc (OId lid) t)
                                                      case t of
                                                        DType TInt 0    -> addInstr (LLStore t (OInteger 0) (OId lid))
                                                        DType TDouble 0 -> addInstr (LLStore t (ODouble 0.0) (OId lid))
                                                        DType TBool 0   -> addInstr (LLStore t (OInteger 0) (OId lid))
                              genDecl (Init x e) = do (_,v) <- genExp e
                                                      lid <- addVar x t
                                                      addInstr (LLAlloc (OId lid) t)
                                                      addInstr (LLStore t v (OId lid))
genStmt (SExp e)            = do  genExp e >> return ()
genStmt (SIncr x)           = do  (lid,t) <- lookupVar x
                                  tr <- createLLVMId
                                  addInstr (LLLoad (OId tr) t lid)    
                                  lid' <- createLLVMId
                                  if t == DType TInt 0
                                    then addInstr (LLAdd (OId lid') t (OId tr) (OInteger 1))
                                    else addInstr (LLAdd (OId lid') t (OId tr) (ODouble 1.0))
                                  addInstr (LLStore t (OId lid') (OId lid))
genStmt (SDecr x)           = do  (lid,t) <- lookupVar x
                                  tr <- createLLVMId
                                  addInstr (LLLoad (OId tr) t lid)    
                                  lid' <- createLLVMId
                                  if t == DType TInt 0
                                    then addInstr (LLSub (OId lid') t (OId tr) (OInteger 1))
                                    else addInstr (LLSub (OId lid') t (OId tr) (ODouble 1.0))
                                  addInstr (LLStore t (OId lid') (OId lid))
genStmt (SIf e s)           = do  (_,v) <- genExp e
                                  ltrue <- createLabel
                                  lfalse <- createLabel
                                  addInstr (LLCBr v ltrue lfalse)
                                  addInstr (LLLabel ltrue)
                                  genStmt s
                                  addInstr (LLBr lfalse)
                                  addInstr (LLLabel lfalse)                                  
genStmt (SIfElse e s1 s2) = do  (_,v) <- genExp e
                                ltrue <- createLabel
                                lfalse <- createLabel
                                lend <- createLabel
                                addInstr (LLCBr v ltrue lfalse)
                                addInstr (LLLabel ltrue)
                                genStmt s1
                                addInstr (LLBr lend)
                                addInstr (LLLabel lfalse)
                                genStmt s2
                                addInstr (LLBr lend)
                                addInstr (LLLabel lend)
genStmt (SWhile e s) = do lbegin <- createLabel
                          addInstr (LLBr lbegin)
                          addInstr (LLLabel lbegin)
                          (_,v) <- genExp e
                          ltrue <- createLabel
                          lfalse <- createLabel
                          addInstr (LLCBr v ltrue lfalse)
                          addInstr (LLLabel ltrue)
                          genStmt s
                          addInstr (LLBr lbegin)
                          addInstr (LLLabel lfalse)
                                   
genExp :: Expr -> S (DType, Op)
genExp (AExpr _ (EId id))     = do  (lid,t) <- lookupVar id
                                    lid' <- createLLVMId
                                    addInstr (LLLoad (OId lid') t lid)
                                    return (t, (OId lid'))
genExp (AExpr t (EInteger i)) = return (t, (OInteger i))
genExp (AExpr t (EDouble  d)) = return (t, (ODouble d))
genExp (AExpr t ETrue)        = return (t, (OInteger 1))
genExp (AExpr t EFalse)       = return (t, (OInteger 0))
genExp (AExpr t (EApp f es))  = do  ps <- mapM genExp es
                                    lid <- createLLVMId
                                    addInstr (LLCall (OId lid) t f ps)
                                    return (t,(OId lid))
genExp (AExpr t (EAppS f str))  = do  id <- addGlobString str 
                                      lid <- createLLVMId
                                      addInstr (LLGetElemPtr (OId lid) ((length str)+1) TString id)
                                      addInstr (LLCall (OId f) TVoid f [(TString,(OId lid))])
                                      return (TVoid,(OId f))

genExp (AExpr (TIdent _) (ENew _ _))  = undefined -- allocate for structs (classes are transformed to structs)
  
genExp (AExpr (DType _ _) (ENew _ es)) = undefined -- allocate arrays

genExp (AExpr t (ENot e)) = do  (_,v) <- genExp e
                                lid <- createLLVMId
                                addInstr (LLXor (OId lid) v (OInteger 1))
                                return (t,(OId lid))
genExp (AExpr t (EAnd e1 e2)) = do  lastLabel <- getLastLabel
                                    (_,v1) <- genExp e1
                                    lfalse <- createLabel
                                    ltrue <- createLabel
                                    addInstr (LLCBr v1 ltrue lfalse)
                                    addInstr (LLLabel ltrue) 
                                    (_,v2) <- genExp e2
                                    lastLabel' <- getLastLabel
                                    lid <- createLLVMId
                                    addInstr (LLAnd (OId lid) v2 v1)
                                    addInstr (LLBr lfalse)
                                    addInstr (LLLabel lfalse)
                                    lid' <- createLLVMId
                                    addInstr (LLPhi (OId lid') (DType TBool 0) (((OInteger 0),lastLabel),((OId lid),lastLabel')) )
                                    return (t,(OId lid'))
genExp (AExpr t (EOr e1 e2)) = do lastLabel <- getLastLabel
                                  (_,v1) <- genExp e1
                                  lfalse <- createLabel
                                  ltrue <- createLabel
                                  addInstr (LLCBr v1 ltrue lfalse)
                                  addInstr (LLLabel lfalse) 
                                  (_,v2) <- genExp e2
                                  lastLabel' <- getLastLabel
                                  lid <- createLLVMId
                                  addInstr (LLOr (OId lid) v2 v1)
                                  addInstr (LLBr ltrue)
                                  addInstr (LLLabel ltrue)
                                  lid' <- createLLVMId
                                  addInstr (LLPhi (OId lid') (DType TBool 0) (((OInteger 1),lastLabel),((OId lid),lastLabel')) )
                                  return (t,(OId lid'))
genExp (AExpr t (ENeg e)) = do  (_,v) <- genExp e
                                lid <- createLLVMId
                                if t == DType TInt 0
                                  then addInstr (LLSub (OId lid) t (OInteger 0) v)
                                  else addInstr (LLSub (OId lid) t (ODouble 0.0) v)
                                return (t,(OId lid))
genExp (AExpr t (EAdd e1 op e2)) = do (_,v1) <- genExp e1
                                      (_,v2) <- genExp e2
                                      lid <- createLLVMId
                                      case op of
                                        Plus -> addInstr (LLAdd (OId lid) t v1 v2)
                                        Minus -> addInstr (LLSub (OId lid) t v1 v2)
                                      return (t, (OId lid))
genExp (AExpr t (EMul e1 op e2)) = do (_,v1) <- genExp e1
                                      (_,v2) <- genExp e2
                                      lid <- createLLVMId
                                      case op of
                                        Mul -> addInstr (LLMul (OId lid) t v1 v2)
                                        Div -> addInstr (LLDiv (OId lid) t v1 v2)
                                        Mod -> addInstr (LLRem (OId lid) t v1 v2)
                                      return (t, (OId lid))
genExp (AExpr _ (ERel e1@(AExpr t _) op e2)) = do (_,v1) <- genExp e1
                                                  (_,v2) <- genExp e2
                                                  lid <- createLLVMId
                                                  if t == DType TInt 0             
                                                    then case op of
                                                          Lth ->  addInstr (LLCmp (OId lid) t "slt" v1 v2)
                                                          Leq ->  addInstr (LLCmp (OId lid) t "sle" v1 v2)
                                                          Gth ->  addInstr (LLCmp (OId lid) t "sgt" v1 v2)
                                                          Geq ->  addInstr (LLCmp (OId lid) t "sge" v1 v2)
                                                          Eq  ->  addInstr (LLCmp (OId lid) t "eq" v1 v2)
                                                          Neq ->  addInstr (LLCmp (OId lid) t "ne" v1 v2)
                                                    else case op of
                                                          Lth ->  addInstr (LLCmp (OId lid) t "olt" v1 v2)
                                                          Leq ->  addInstr (LLCmp (OId lid) t "ole" v1 v2)
                                                          Gth ->  addInstr (LLCmp (OId lid) t "ogt" v1 v2)
                                                          Geq ->  addInstr (LLCmp (OId lid) t "oge" v1 v2)
                                                          Eq  ->  addInstr (LLCmp (OId lid) t "oeq" v1 v2)
                                                          Neq ->  addInstr (LLCmp (OId lid) t "one" v1 v2)
                                                  return (DType TBool 0,(OId lid))
genExp e = error (show e)
