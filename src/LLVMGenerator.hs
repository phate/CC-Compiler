module LLVMGenerator where

import ParserAbs
import LLVMEnv
import LLVMAbs
import Control.Monad.State

generateInstructions :: Program -> [LLVMEnv]
generateInstructions (Program defs) = genDefs defs

genDefs :: [FctDef] -> [LLVMEnv]
genDefs []      = []
genDefs (d:ds)  = (execState (genDef d) newEnv):(genDefs ds)

genDef :: FctDef -> S()
genDef (FctDef t id args (CStmt ss)) = do setFunctionName id
                                          setReturnType t
                                          let ps = [(i,t) | (Arg t i) <- args]
                                          setParameters ps
                                          mapM_ (\(x, t) -> addVar x t) ps
                                          mapM_ genParams ps 
                                          genStmts ss
                                          s <- get
                                          let (i:is) = instr s
                                          case i of
                                            (LLLabel "lab0") -> put $ s { instr = LLVReturn:is }
                                            (LLLabel _) -> put $ s { instr = Unreachable:i:is }
                                            _           -> return()
                                        where
                                          genParams (i,t) = do  (lid,_) <- lookupVar i
                                                                let plid = createPLLVMId lid
                                                                addInstr (LLAlloc (OL lid) t)
                                                                addInstr (LLStore t (OL plid) (OL lid))

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
genStmt (SAss x e)          = do  (lid,t) <- lookupVar x
                                  (_,v) <- genExp e
                                  addInstr (LLStore t v (OL lid))
genStmt (SDecl t vs) = do   mapM_ genDecl vs
                            where
                              genDecl (NoInit x) = do lid <- addVar x t
                                                      addInstr (LLAlloc (OL lid) t)
                                                      case t of
                                                        TInt -> addInstr (LLStore t (OI 0) (OL lid))
                                                        TDouble -> addInstr (LLStore t (OD 0.0) (OL lid))
                                                        TBool -> addInstr (LLStore t (OI 0) (OL lid))
                              genDecl (Init x e) = do (_,v) <- genExp e
                                                      lid <- addVar x t
                                                      addInstr (LLAlloc (OL lid) t)
                                                      addInstr (LLStore t v (OL lid))
genStmt (SExp e)            = do  genExp e >> return ()
genStmt (SIncr x)           = do  (lid,t) <- lookupVar x
                                  tr <- createLLVMId
                                  addInstr (LLLoad (OL tr) t lid)    
                                  lid' <- createLLVMId
                                  if t == TInt
                                    then addInstr (LLAdd (OL lid') t (OL tr) (OI 1))
                                    else addInstr (LLAdd (OL lid') t (OL tr) (OD 1.0))
                                  addInstr (LLStore t (OL lid') (OL lid))
genStmt (SDecr x)           = do  (lid,t) <- lookupVar x
                                  tr <- createLLVMId
                                  addInstr (LLLoad (OL tr) t lid)    
                                  lid' <- createLLVMId
                                  if t == TInt
                                    then addInstr (LLSub (OL lid') t (OL tr) (OI 1))
                                    else addInstr (LLSub (OL lid') t (OL tr) (OD 1.0))
                                  addInstr (LLStore t (OL lid') (OL lid))
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
                                   
genExp :: Expr -> S (Type, Op)
genExp (AExpr _ (EId id))     = do  (lid,t) <- lookupVar id
                                    lid' <- createLLVMId
                                    addInstr (LLLoad (OL lid') t lid)
                                    return (t, (OL lid'))
genExp (AExpr t (EInteger i)) = return (t, (OI i))
genExp (AExpr t (EDouble  d)) = return (t, (OD d))
genExp (AExpr _ ETrue)        = return (TBool, (OI 1))
genExp (AExpr _ EFalse)       = return (TBool, (OI 0))
genExp (AExpr t (EApp f es))  = do  ps <- mapM genExp es
                                    lid <- createLLVMId
                                    addInstr (LLCall (OL lid) t f ps)
                                    return (t,(OL lid))
genExp (AExpr t (EAppS f str))  = do  s <- get
                                      let gc = globConst s
                                      let id = ("_" ++ (funName s) ++ "string" ++ (show (length gc)))
                                      addGlobConst id str
                                      lid <- createLLVMId
                                      addInstr (LLGetElemPtr (OL lid) ((length str)+1) TString id)
                                      addInstr (LLCall (OL f) TVoid f [(TStringP,(OL lid))])
                                      return (TVoid,(OL f))

genExp (AExpr t (ENot e)) = do  (_,v) <- genExp e
                                lid <- createLLVMId
                                addInstr (LLXor (OL lid) v (OI 1))
                                return (t,(OL lid))
genExp (AExpr t (EAnd e1 e2)) = do  lastLabel <- getLastLabel
                                    (_,v1) <- genExp e1
                                    lfalse <- createLabel
                                    ltrue <- createLabel
                                    addInstr (LLCBr v1 ltrue lfalse)
                                    addInstr (LLLabel ltrue) 
                                    (_,v2) <- genExp e2
                                    lastLabel' <- getLastLabel
                                    lid <- createLLVMId
                                    addInstr (LLAnd (OL lid) v2 v1)
                                    addInstr (LLBr lfalse)
                                    addInstr (LLLabel lfalse)
                                    lid' <- createLLVMId
                                    addInstr (LLPhi (OL lid') TBool (((OI 0),lastLabel),((OL lid),lastLabel')) )
                                    return (t,(OL lid'))
genExp (AExpr t (EOr e1 e2)) = do lastLabel <- getLastLabel
                                  (_,v1) <- genExp e1
                                  lfalse <- createLabel
                                  ltrue <- createLabel
                                  addInstr (LLCBr v1 ltrue lfalse)
                                  addInstr (LLLabel lfalse) 
                                  (_,v2) <- genExp e2
                                  lastLabel' <- getLastLabel
                                  lid <- createLLVMId
                                  addInstr (LLOr (OL lid) v2 v1)
                                  addInstr (LLBr ltrue)
                                  addInstr (LLLabel ltrue)
                                  lid' <- createLLVMId
                                  addInstr (LLPhi (OL lid') TBool (((OI 1),lastLabel),((OL lid),lastLabel')) )
                                  return (t,(OL lid'))
genExp (AExpr t (ENeg e)) = do  (_,v) <- genExp e
                                lid <- createLLVMId
                                if t == TInt
                                  then addInstr (LLSub (OL lid) t (OI 0) v)
                                  else addInstr (LLSub (OL lid) t (OD 0.0) v)
                                return (t,(OL lid))
genExp (AExpr t (EAdd e1 op e2)) = do (_,v1) <- genExp e1
                                      (_,v2) <- genExp e2
                                      lid <- createLLVMId
                                      case op of
                                        Plus -> addInstr (LLAdd (OL lid) t v1 v2)
                                        Minus -> addInstr (LLSub (OL lid) t v1 v2)
                                      return (t, (OL lid))
genExp (AExpr t (EMul e1 op e2)) = do (_,v1) <- genExp e1
                                      (_,v2) <- genExp e2
                                      lid <- createLLVMId
                                      case op of
                                        Mul -> addInstr (LLMul (OL lid) t v1 v2)
                                        Div -> addInstr (LLDiv (OL lid) t v1 v2)
                                        Mod -> addInstr (LLRem (OL lid) t v1 v2)
                                      return (t, (OL lid))
genExp (AExpr _ (ERel e1@(AExpr t _) op e2)) = do (_,v1) <- genExp e1
                                                  (_,v2) <- genExp e2
                                                  lid <- createLLVMId
                                                  if t == TInt              
                                                    then case op of
                                                          Lth ->  addInstr (LLCmp (OL lid) t "slt" v1 v2)
                                                          Leq ->  addInstr (LLCmp (OL lid) t "sle" v1 v2)
                                                          Gth ->  addInstr (LLCmp (OL lid) t "sgt" v1 v2)
                                                          Geq ->  addInstr (LLCmp (OL lid) t "sge" v1 v2)
                                                          Eq  ->  addInstr (LLCmp (OL lid) t "eq" v1 v2)
                                                          Neq ->  addInstr (LLCmp (OL lid) t "ne" v1 v2)
                                                    else case op of
                                                          Lth ->  addInstr (LLCmp (OL lid) t "olt" v1 v2)
                                                          Leq ->  addInstr (LLCmp (OL lid) t "ole" v1 v2)
                                                          Gth ->  addInstr (LLCmp (OL lid) t "ogt" v1 v2)
                                                          Geq ->  addInstr (LLCmp (OL lid) t "oge" v1 v2)
                                                          Eq  ->  addInstr (LLCmp (OL lid) t "oeq" v1 v2)
                                                          Neq ->  addInstr (LLCmp (OL lid) t "one" v1 v2)
                                                  return (TBool,(OL lid))
genExp e = error (show e)
