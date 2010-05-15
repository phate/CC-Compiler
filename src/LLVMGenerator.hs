module LLVMGenerator where

import ParserAbs
import LLVMEnv
import LLVMAbs
import Control.Monad.State

generateInstructions :: Program -> LLVMEnv
generateInstructions (Program defs) = execState (genDefs defs) newLLVMEnv


genDefs :: [Def] -> S()
genDefs defs = do mapM_ insertStruct [ s | (SDef s) <- defs ]
                  mapM_ genFctDef [ f | (FDef f) <- defs ]
                  return ()
  where
    insertStruct (StrDef id decls) = addStruct id [ (id', t) | (t, id') <- decls ]


--genDefs :: [Def] -> S()
--genDefs []            = return ()
--genDefs ((FDef f):ds) = genFctDef f

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
genStmt (SAss x [] e)       = do  (lid,t) <- lookupVar x
                                  (_,v) <- genExp e
                                  addInstr (LLStore t v (OId lid))

genStmt (SAss x (es:[]) e)  = do (lid, (DType t _)) <- lookupVar x
                                 lid' <- createLLVMId
                                 addInstr (LLLoad (OId lid') (DType t 1) lid)
                                 (t', v) <- genExp e
                                 lid'' <- createLLVMId
                                 addInstr (LLGetElemPtr (OId lid'') (DType t 1) (OId lid') (DType TInt 0) (OInteger 1))
                                 (t'', v') <- genExp es
                                 lid''' <- createLLVMId
                                 addInstr (LLGetElemPtr (OId lid''') (TArr t) (OId lid'') (DType TInt 0) v')
                                 addInstr (LLStore t' v (OId lid'''))

genStmt (SDerf e1 field e2) = do (t@(TIdent id),v) <- genExp e1
                                 fOff <- getFieldOffset field id
                                 lid' <- createLLVMId
                                 addInstr (LLGetElemPtr (OId lid') t v (DType TInt 0) (OInteger fOff))
                                 (t',v') <- genExp e2
                                 addInstr (LLStore t' v' (OId lid'))



genStmt (SDecl t vs) = do   mapM_ genDecl vs
                            where
                              genDecl (NoInit x) = do lid <- addVar x t
                                                      addInstr (LLAlloc (OId lid) t)
                                                      case t of
                                                        DType TInt 0    -> addInstr (LLStore t (OInteger 0) (OId lid))
                                                        DType TDouble 0 -> addInstr (LLStore t (ODouble 0.0) (OId lid))
                                                        DType TBool 0   -> addInstr (LLStore t (OInteger 0) (OId lid))
                                                        TIdent i        -> addInstr (LLStore t ONull (OId lid))
                                                        _               -> return () -- For arrays, might instead store null?
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

genStmt (SFor (DType t i) id e@(AExpr (DType TInt i2) (EId eid)) s) = 
  genStmts [
  (SDecl (DType TInt 0) [Init "_iC" (AExpr (DType TInt 0) (EInteger 0))]), -- int _iC = 0
  (SWhile (AExpr (DType TBool 0) (ERel (AExpr (DType TInt 0) (EId "_iC")) Lth (AExpr (DType TInt 0) (EDot e (EId "length")))))) (SCStmt (CStmt [ --(while _iC < e.length)
  (SDecl (DType t i) [NoInit id]), -- int ([]) x;
  (SAss id [] (AExpr (DType t i) (EIdx eid [(AExpr (DType TInt 0) (EId "_iC"))]))), -- id = e[_iC]
  s, -- statements
  (SIncr "_iC")]))] -- _iC++

                                   
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
                                      addInstr (LLGetElemPtrString (OId lid) ((length str)+1) TString id)
                                      addInstr (LLCall (OId f) TVoid f [(TPtr8,(OId lid))])
                                      return (TVoid,(OId f))

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
genExp (AExpr _ (ERel e1@(AExpr t _) op e2)) = 
  do (_,v1) <- genExp e1
     (_,v2) <- genExp e2
     lid <- createLLVMId
     case t of
       DType TDouble _ -> case op of
                            Lth ->  addInstr (LLCmp (OId lid) t "olt" v1 v2)
                            Leq ->  addInstr (LLCmp (OId lid) t "ole" v1 v2)
                            Gth ->  addInstr (LLCmp (OId lid) t "ogt" v1 v2)
                            Geq ->  addInstr (LLCmp (OId lid) t "oge" v1 v2)
                            Eq  ->  addInstr (LLCmp (OId lid) t "oeq" v1 v2)
                            Neq ->  addInstr (LLCmp (OId lid) t "one" v1 v2)
       
       _               -> case op of
                            Lth ->  addInstr (LLCmp (OId lid) t "slt" v1 v2)
                            Leq ->  addInstr (LLCmp (OId lid) t "sle" v1 v2)
                            Gth ->  addInstr (LLCmp (OId lid) t "sgt" v1 v2)
                            Geq ->  addInstr (LLCmp (OId lid) t "sge" v1 v2)
                            Eq  ->  addInstr (LLCmp (OId lid) t "eq" v1 v2)
                            Neq ->  addInstr (LLCmp (OId lid) t "ne" v1 v2)                                  
     return (DType TBool 0,(OId lid))



genExp (AExpr t@(TIdent id) (ENew _ _))  = do lid <- createLLVMId
                                              size <- getStructSize id
                                              addInstr (LLCall (OId lid) (TPtr8) "calloc" [((DType TInt 0), OInteger size), ((DType TInt 0), OInteger 1)])
                                              lid' <- createLLVMId
                                              addInstr (LLBitcast (OId lid') (TPtr8) (OId lid) t)
                                              return (t, (OId lid'))


genExp (AExpr t (ENull ptr)) = return (t, ONull)

genExp (AExpr t (EPtr e field)) = do (t'@(TIdent id),v) <- genExp e
                                     fOff <- getFieldOffset field id
                                     lid' <- createLLVMId
                                     addInstr (LLGetElemPtr (OId lid') t' v (DType TInt 0) (OInteger fOff))
                                     lid'' <- createLLVMId
                                     addInstr (LLLoad (OId lid'') t lid')
                                     return (t, (OId lid''))
  
genExp (AExpr t@(DType arrT dim) (ENew _ (e:[]))) = do (t', v) <- genExp e
                                                       lid <- createLLVMId
                                                       case arrT of
                                                         TBool -> addInstr (LLMul (OId lid) (DType TInt 0) (OInteger 1) v)
                                                         TInt  -> addInstr (LLMul (OId lid) (DType TInt 0) (OInteger 4) v)
                                                         TDouble -> addInstr (LLMul (OId lid) (DType TInt 0) (OInteger 8) v)
                                                       lid' <- createLLVMId
                                                       addInstr (LLAdd (OId lid') (DType TInt 0) (OInteger 4) (OId lid)) 
                                                       lid'' <- createLLVMId
                                                       addInstr (LLCall (OId lid'') (TPtr8) "calloc" [((DType TInt 0), (OId lid')), ((DType TInt 0), OInteger 1)])
                                                       lid''' <- createLLVMId
                                                       addInstr (LLBitcast (OId lid''') (TPtr8) (OId lid'') t)
                                                       lid'''' <- createLLVMId -- STORING LENGTH
                                                       addInstr (LLGetElemPtr (OId lid'''') t (OId lid''') (DType TInt 0) (OInteger 0))
                                                       addInstr (LLStore t' v (OId lid'''')) -- END STORING LENGTH
                                                       return (t, (OId lid'''))
                
-- TODO: SHOULD NOT BE ID BUT INSTEAD EXPRESSION AFTER EIdx, THIS SEEMS TO WORK OTHERWISE                             
genExp (AExpr t@(DType arrT dim) (EIdx id (e:[]))) = do (lid,t') <- lookupVar id
                                                        lid' <- createLLVMId
                                                        addInstr (LLLoad (OId lid') t' lid) 
                                                        (t'', v) <- genExp e
                                                        lid'' <- createLLVMId
                                                        addInstr (LLGetElemPtr (OId lid'') t' (OId lid') (DType TInt 0) (OInteger 1))
                                                        lid''' <- createLLVMId
                                                        addInstr (LLGetElemPtr (OId lid''') (TArr arrT) (OId lid'') (DType TInt 0) v)
                                                        lid'''' <- createLLVMId
                                                        addInstr (LLLoad (OId lid'''') t lid''')
                                                        return (t, (OId lid''''))

genExp (AExpr t@(DType TInt 0) (EDot e (EId "length"))) =
  do (t', v) <- genExp e
     lid <- createLLVMId
     addInstr (LLGetElemPtr (OId lid) t' v (DType TInt 0) (OInteger 0))
     lid' <- createLLVMId
     addInstr (LLLoad (OId lid') t lid)
     return (t, (OId lid'))
     



genExp e = error $ "ERROR: " ++ (show e)
