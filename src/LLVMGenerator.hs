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

genFctDef :: FctDef -> S()
genFctDef (FctDef t id args (CStmt ss)) = do  let ps = [ (t,i) | Arg t i <- args]
                                              newFct t id ps
                                              addParams ps
                                              mapM_ genParams ps
                                              genStmts ss
                                              linstr <- getLastInstr
                                              case linstr of
                                                (LLLabel "lab0")  -> addInstr LLVReturn
                                                (LLLabel _)       -> if t == TVoid then (addInstr LLVReturn) else (addInstr Unreachable) -- Naive for void fct
                                                _                 -> if t == TVoid then (addInstr LLVReturn) else return () -- Naive for void fct
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
                                  
                                  -- Subtyping (Naive since it also bitcasts i32 to i32 etc)
                                  ret <- getReturnType
                                  lid' <- createLLVMId
                                  addInstr (LLBitcast (OId lid') t v ret)
                                  addInstr (LLReturn ret (OId lid'))

genStmt SVRet               = do  addInstr LLVReturn


-- Pattern matching on LHS of assignments
-- Normal ID's
genStmt (SAss (AExpr t (EId id)) e2) = do (lid,_) <- lookupVar id
                                  	  (t',v) <- genExp e2

                                           -- Subtyping (Naive since it also bitcasts i32 to i32 etc)
                                          lid' <- createLLVMId
                                          addInstr (LLBitcast (OId lid') t' v t)
                                          addInstr (LLStore t (OId lid') (OId lid))

-- EIndex, ie a[3] = 1;. (int a[][], int[] b): a[3] = b etc
genStmt (SAss e1@(AExpr t (EIndex eid@(AExpr _ (EId id)) es)) e2) =
  do (t',(OId v1)) <- genExp eid
     lid <- sAssEId' v1 t' es
     (_,v2) <- genExp e2
     addInstr (LLStore t v2 (OId lid))
 
  where
    sAssEId' :: LLVMId -> DType -> [Expr] -> S LLVMId
    sAssEId' lid t@(DType arrT i) (e:[]) = 
      do lid' <- createLLVMId
         addInstr (LLGetElemPtr (OId lid') t (OId lid) (DType TInt 0) (OInteger 1))
         (_, v) <- genExp e
         lid'' <- createLLVMId
         addInstr (LLGetElemPtr (OId lid'') (TArr arrT (i-1)) (OId lid') (DType TInt 0) v)
         return lid''
    sAssEId' lid t@(DType arrT i) (e:es) =
      do lid' <- createLLVMId
         addInstr (LLGetElemPtr (OId lid') t (OId lid) (DType TInt 0) (OInteger 1))
         (_, v) <- genExp e
         lid'' <- createLLVMId
         addInstr (LLGetElemPtr (OId lid'') (TArr arrT (i-1)) (OId lid') (DType TInt 0) v)
         lid''' <- createLLVMId
         addInstr (LLLoad (OId lid''') (DType arrT (i-1)) lid'')
         sAssEId' lid''' (DType arrT (i-1)) es

     

-- Pointer dereferencing
genStmt (SAss (AExpr t (EPtr e1 field)) e2) = do (t'@(TIdent id),v) <- genExp e1
                                                 fOff <- getFieldOffset field id
                                                 lid' <- createLLVMId
                                                 addInstr (LLGetElemPtr (OId lid') t' v (DType TInt 0) (OInteger fOff))
                                                 (t'',v') <- genExp e2
                                                 addInstr (LLStore t'' v' (OId lid'))




genStmt (SDecl t vs) = do   mapM_ genDecl vs
                            where
                              genDecl (NoInit x) = do lid <- addVar x t
                                                      addInstr (LLAlloc (OId lid) t)
                                                      case t of
                                                        DType TInt 0    -> addInstr (LLStore t (OInteger 0) (OId lid))
                                                        DType TDouble 0 -> addInstr (LLStore t (ODouble 0.0) (OId lid))
                                                        DType TBool 0   -> addInstr (LLStore t (OInteger 0) (OId lid))
                                                        TIdent i        -> addInstr (LLStore t ONull (OId lid))
                                                        _               -> return () -- For arrays, might instead store null, doesnt seem to matter though.
                              genDecl (Init x e) = do (t',v) <- genExp e
                                                      lid <- addVar x t
                                                      addInstr (LLAlloc (OId lid) t)

                                                      -- Subtyping (Naive since it also bitcasts i32 to i32 etc)
                                                      lid' <- createLLVMId
                                                      addInstr (LLBitcast (OId lid') t' v t)
                                                      addInstr (LLStore t (OId lid') (OId lid))

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

genStmt (SFor t id e@(AExpr t' _) s) = 
  do pushScope
     genStmts [
       (SDecl (DType TInt 0) [Init "_iC" (AExpr (DType TInt 0) (EInteger 0))]), -- int _iC = 0
       (SDecl t' [Init "_array" e]), -- store array
       (SDecl (DType TInt 0) [Init "_length" (AExpr (DType TInt 0) (EDot (AExpr t' (EId "_array")) (EId "length")))]), -- int _length = _array.length
       (SWhile (AExpr (DType TBool 0) (ERel (AExpr (DType TInt 0) (EId "_iC")) Lth (AExpr (DType TInt 0) (EId "_length"))))) (SCStmt (CStmt [ --(while _iC < _length)
       (SDecl t [Init id (AExpr t (EIndex (AExpr t' (EId "_array")) [(AExpr (DType TInt 0) (EId "_iC"))]))]), -- int([]) id = _array([])[_iC]
       s, -- statements
       (SIncr "_iC")]))] -- _iC++
     popScope
     return ()

                                   
genExp :: Expr -> S (DType, Op)
genExp (AExpr _ (EId id))     = do  (lid,t) <- lookupVar id
                                    lid' <- createLLVMId
                                    addInstr (LLLoad (OId lid') t lid)
                                    return (t, (OId lid'))
genExp (AExpr t (EInteger i)) = return (t, (OInteger i))
genExp (AExpr t (EDouble  d)) = return (t, (ODouble d))
genExp (AExpr t ETrue)        = return (t, (OInteger 1))
genExp (AExpr t EFalse)       = return (t, (OInteger 0))

genExp (AExpr t (EAppTypes f es ts))  = 
  do ps <- genEAppTypes es ts
     lid <- createLLVMId
     addInstr (LLCall (OId lid) t f ps)
     return (t,(OId lid))
  where -- Subtyping
    genEAppTypes [] []             = return []
    genEAppTypes (e':es') (t':ts') = do (t'',v) <- genExp e'
                                        lid' <- createLLVMId
                                        addInstr (LLBitcast (OId lid') t'' v t')
                                        aes <- genEAppTypes es' ts'
                                        return ((t', (OId lid')):aes)
                                        

genExp (AExpr t (EAppS f str))  = do  id <- addGlobString str 
                                      lid <- createLLVMId
                                      addInstr (LLGetElemPtrString (OId lid) ((length str)+1) TString id)
                                      addInstr (LLCall (OId f) TVoid f [(TPtr8,(OId lid))])
                                      return (TVoid,(OId f))

genExp (AExpr t (ENot e)) = do  (_,v) <- genExp e
                                lid <- createLLVMId
                                addInstr (LLXor (OId lid) v (OInteger 1))
                                return (t,(OId lid))
genExp (AExpr t (EAnd e1 e2)) = do  (_,v1) <- genExp e1
                                    lastLabel <- getLastLabel
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
genExp (AExpr t (EOr e1 e2)) = do (_,v1) <- genExp e1
                                  lastLabel <- getLastLabel
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

-- For single dimensional arrays
genExp (AExpr t@(DType arrT dim) (ENew _ (e:[]))) =
  do (_, v) <- genExp e
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
     addInstr (LLStore (DType TInt 0) v (OId lid'''')) -- END STORING LENGTH
     return (t, (OId lid'''))

-- For multidimensional arrays, need to loop here
genExp (AExpr t@(DType arrT dim) (ENew e' (e:es))) =
  do (_, v) <- genExp e
     -- First calloc, store etc
     lid <- createLLVMId
     addInstr (LLMul (OId lid) (DType TInt 0) (OInteger 4) v) -- Pointer size 4
     lid' <- createLLVMId
     addInstr (LLAdd (OId lid') (DType TInt 0) (OInteger 4) (OId lid)) -- + 4 for i32 length
     lid'' <- createLLVMId
     addInstr (LLCall (OId lid'') TPtr8 "calloc" [((DType TInt 0), (OId lid')), ((DType TInt 0), OInteger 1)]) -- allocate heap mem
     lid''' <- createLLVMId
     addInstr (LLBitcast (OId lid''') TPtr8 (OId lid'') t)
     lid'''' <- createLLVMId
     addInstr (LLGetElemPtr (OId lid'''') t (OId lid''') (DType TInt 0) (OInteger 0)) -- length
     addInstr (LLStore (DType TInt 0) v (OId lid''''))

     -- Then do this iteratively for all arrays inside this one
     cmpLbl <- createLabel
     counter <- createLLVMId
     addInstr (LLAlloc (OId counter) (DType TInt 0))
     addInstr (LLStore (DType TInt 0) (OInteger 0) (OId counter)) -- store 0 in counter
     addInstr (LLBr cmpLbl)
     addInstr (LLLabel cmpLbl)
     counterId <- createLLVMId
     addInstr (LLLoad (OId counterId) (DType TInt 0) counter)
     cmpLid <- createLLVMId
     addInstr (LLCmp (OId cmpLid) (DType TInt 0) "slt" (OId counterId) v)
     ltrue <- createLabel
     lfalse <- createLabel
     addInstr (LLCBr (OId cmpLid) ltrue lfalse)
     addInstr (LLLabel ltrue)

     -- Inside loop         
     (t', v2) <- genExp (AExpr (DType arrT (dim-1)) (ENew e' es)) -- Gen subarray
     ptrLid <- createLLVMId
     arrayLid <- createLLVMId
     addInstr (LLGetElemPtr (OId ptrLid) t (OId lid''') (DType TInt 0) (OInteger 1)) -- get array
     addInstr (LLGetElemPtr (OId arrayLid) (TArr arrT (dim-1)) (OId ptrLid) (DType TInt 0) (OId counterId)) -- Get array[counter]
     addInstr (LLStore t' v2 (OId arrayLid)) -- Store array
     

     -- Increment counter and go to test
     countInc <- createLLVMId
     addInstr (LLAdd (OId countInc) (DType TInt 0) (OId counterId) (OInteger 1))
     addInstr (LLStore (DType TInt 0) (OId countInc) (OId counter))
     addInstr (LLBr cmpLbl)

     -- Outside loop, done
     addInstr (LLLabel lfalse)
     return (t, (OId lid'''))


genExp (AExpr t (EIndex e es)) =
  do (t', (OId v)) <- genExp e
     lid' <- eIdxRec v t' es
     return (t, (OId lid'))
     

genExp (AExpr t@(DType TInt 0) (EDot e (EId "length"))) =
  do (t', v) <- genExp e
     lid <- createLLVMId
     addInstr (LLGetElemPtr (OId lid) t' v (DType TInt 0) (OInteger 0))
     lid' <- createLLVMId
     addInstr (LLLoad (OId lid') t lid)
     return (t, (OId lid'))


genExp e = error $ "ERROR: " ++ (show e)

eIdxRec :: LLVMId -> DType -> [Expr] -> S LLVMId
eIdxRec lid _ [] = return lid
eIdxRec lid t@(DType arrT i) (e:es) = 
  do lid' <- createLLVMId
     addInstr (LLGetElemPtr (OId lid') t (OId lid) (DType TInt 0) (OInteger 1))
     (_, v) <- genExp e
     lid'' <- createLLVMId
     addInstr (LLGetElemPtr (OId lid'') (TArr arrT (i-1)) (OId lid') (DType TInt 0) v)
     lid''' <- createLLVMId
     addInstr (LLLoad (OId lid''') (DType arrT (i-1)) lid'')
     eIdxRec lid''' (DType arrT (i-1)) es

