module TypeChecker where

import TypeCheckerEnv
import ParserAbs
import Control.Monad.State
import ErrM

typecheck :: Program -> Err (Program, TCEnv)
typecheck (Program defs) = do (defs', env) <- runStateT (setupDefs defs) newTCEnv
                              return ((Program defs'), env)


setupDefs :: [Def] -> S [Def]
setupDefs ds = do let strDefs  = [ s | (SDef s) <- ds ]
                  let typeDefs = [ td | (TDef td) <- ds ]
                  let cDefs    = [ c | (CDef c@(ClassDef id cdecls)) <- ds ]
                  let eCDefs   = [ ec | (CDef ec@(EClassDef sub base cdecls)) <- ds ]
                  let fctDefs  = [ f | (FDef f) <- ds ]


                  mapM_ insertStruct strDefs
                  mapM_ insertTypeDef typeDefs
                  mapM_ insertClass cDefs
                  mapM_ insertSubClass eCDefs
                  mapM_ insertFunction fctDefs
                  mapM_ insertClassMethods cDefs
                  mapM_ insertSubClassMethods eCDefs
                  checkDefs ds
	where  
               insertStruct (StrDef id decls)             = addStruct id [ (id', t) | (t, id') <- decls ]
               insertTypeDef (TypeDef struct ptr)         = addPtrTypeDef struct ptr

               insertClass (ClassDef id cdecls)           = addClass id [ (id',t) | (CDeclA (t,id')) <- cdecls]
               insertSubClass (EClassDef sub base cdecls) = addSubClass sub base [ (id',t) | (CDeclA (t,id')) <- cdecls]

               insertClassMethods (ClassDef cid cdecls)   = mapM_ (insertClassMethod cid) [ (mid, t, args) | (CDeclM (FctDef t mid args _)) <- cdecls]
               insertSubClassMethods (EClassDef sub base cdecls) = mapM_ (insertClassMethod sub) [ (mid, t, args) | (CDeclM (FctDef t mid args _)) <- cdecls]
               insertClassMethod cid (mid, ret, args)     = addMethod cid mid ([t' | (Arg t' id') <- args ], ret)

               insertFunction (FctDef t id args ss)       = addFun id ([t' | (Arg t' id') <- args], t)

checkDefs :: [Def] -> S [Def]
checkDefs []            = do  mainFun <- lookupFun "main"
                              case mainFun of
                                ([], DType TInt 0)  -> return []
                                _                   -> fail $ "wrong definition of main function"
checkDefs ((FDef f):ds) = do  fdef  <- checkFctDef f
                              ds'   <- checkDefs ds
                              return $ (FDef fdef):ds'

checkDefs ((SDef s):ds) = do  sdef <- checkStrDef s
                              ds'  <- checkDefs ds
                              return $ (SDef sdef):ds'

checkDefs ((CDef c):ds) = do  cdef <- checkClassDef c
                              ds' <- checkDefs ds
                              return $ (CDef cdef):ds'

checkDefs (d:ds)        = do  ds' <- checkDefs ds
                              return $ d:ds'


checkClassDef :: ClassDef -> S ClassDef
checkClassDef (ClassDef id cdecls) =
  do setCheckingClass True id
     -- Check no multiple variables
     iVars <- getAllInstanceVariables id
     emptyContext
     mapM_ (\(t',x) -> addVar t' x True) iVars

     let meths = [ f | f@(CDeclM (FctDef t id' args (CStmt ss))) <- cdecls ]
     meths' <- mapM (checkClassMeth id iVars) meths
     iVars' <- resolveInstanceVariableTypes iVars
     let iVars' = [ CDeclA (t,id') | (t,id') <- iVars ]
     return (ClassDef id (iVars' ++ meths'))

checkClassDef (EClassDef sub base cdecls) =
  do setCheckingClass True sub
     -- Check no multiple variables
     iVars <- getAllInstanceVariables sub -- Also returns parents variables, in correct order, i.e. 1st parent 1st etc, for codegenerator later.
     emptyContext
     mapM_ (\(t',x) -> addVar t' x True) iVars

     checkMethodsNotOverride sub
     let meths = [ f | f@(CDeclM (FctDef t id' args (CStmt ss))) <- cdecls ]
     meths' <- mapM (checkClassMeth sub iVars) meths
     iVars' <- resolveInstanceVariableTypes iVars
     return (EClassDef sub base (iVars' ++ meths'))

resolveInstanceVariableTypes :: [(DType, Id)] -> S [CDecl]
resolveInstanceVariableTypes iVars =
  do let attrT = [ t | (t, id') <- iVars ]
     let attrId = [ id' | (t, id') <- iVars ]
     attrT' <- mapM resolveType attrT
     let zipped = zip attrT' attrId
     let resolved = [ CDeclA (t,id') | (t,id') <- zipped ]
     return resolved
     

-- ClassName -> Instance variables -> Method
checkClassMeth :: Id -> [(DType, Id)] -> CDecl -> S CDecl
checkClassMeth cid iVars (CDeclM (FctDef t mid args (CStmt ss))) = 
  do emptyContext
     setReturnType t
     mapM_ (\(t',x) -> addVar t' x True) iVars -- Instance variables

     pushScope
     mapM_ (\(t',x) -> addVar t' x False) [(t', x) | (Arg t' x) <- args] -- Parameters
     (ps,rt) <- lookupMethod cid mid
     let args' = zipWith Arg ps [id | (Arg _ id) <- args]
     (retOk, ss') <- checkStmts ss False
     popScope
     if retOk || (t == TVoid)
       then return (CDeclM (FctDef rt mid args' (CStmt ss')))
       else fail $ "Method " ++ mid ++ " in class " ++ cid ++ " does not return"

checkStrDef :: StrDef -> S StrDef
checkStrDef (StrDef id decls) = 
  do emptyContext
     (_,decls') <- checkStmts [ SDecl t [NoInit id'] | (t, id') <- decls ] True
     let decls'' = [ (t, id') | (SDecl t [NoInit id']) <- decls' ]
     return (StrDef id decls'')
                                    

checkFctDef :: FctDef -> S FctDef
checkFctDef (FctDef t id args (CStmt ss)) =
   do emptyContext
      setCheckingClass False ""
      setReturnType t
      mapM_ (\(t,x) -> addVar t x False) [(t',id') | (Arg t' id') <- args]
      (ps,rt) <- lookupFun id
      let args' = zipWith Arg ps [ id | (Arg _ id) <- args]
      (retOk, ss') <- checkStmts ss False
      if retOk || (t == TVoid)
        then return (FctDef rt id args' (CStmt ss'))
        else fail $ "Function " ++ id ++ " does not return"


checkStmts :: [Stmt] -> Bool -> S (Bool,[Stmt])
checkStmts [] ret     = return (ret,[])
checkStmts (s:ss) ret = do (ret', s') <- checkStmt s ret
                           (ret'',ss') <- checkStmts ss (ret || ret')
                           return (ret'', s':ss')	

checkStmt :: Stmt -> Bool -> S (Bool,Stmt)
checkStmt SEmpty ret              = return (ret,SEmpty)
checkStmt (SCStmt (CStmt ss)) ret = do  pushScope
                                        (ret', ss') <- checkStmts ss ret
                                        popScope
                                        return ( ret || ret', SCStmt (CStmt ss'))
checkStmt (SDecl t vars) ret      = do 	t' <- resolveType t
                                        vars' <- mapM checkDecl vars
                                        return (ret, (SDecl t' vars')) where
                                          checkDecl (NoInit id)     = do  addVar t id False
                                                                          return (NoInit id)
                                          checkDecl x@(Init id exp) = do  t' <- resolveType t
									  ae <- checkExpr [t'] exp (show x)
                                                                          addVar t id False
                                                                          return (Init id ae)

checkStmt s@(SAss e1@(EId id) e2) ret = do  ae1@(AExpr t _) <- inferExpr e1
                                            ae2 <- checkExpr [t] e2 (show s)
                                            return (ret, (SAss ae1 ae2))

checkStmt s@(SAss e1@(EIndex (EId id) es) e2) ret =
  do ae1@(AExpr t _) <- inferExpr e1
     ae2 <- checkExpr [t] e2 (show s)
     return (ret, (SAss ae1 ae2))

checkStmt s@(SAss e1@(EPtr e f) e2) ret = do ae1@(AExpr t _) <- inferExpr e1
                                             ae2 <- checkExpr [t] e2 (show s)
                                             return (ret, (SAss ae1 ae2))

checkStmt s@(SAss _ _) ret = fail $ (show s) ++ ": Invalid l-value in assignment"
                                        
                                          
checkStmt (SExp e) ret            = do  ae <- inferExpr e
                                        return (ret, SExp ae)
checkStmt s@(SIncr x) ret         = do  checkVar (DType TInt 0) x (show s)
                                        (_,b) <- lookupVar x
                                        return (ret, SIncrBool x b)
checkStmt s@(SDecr x) ret         = do  checkVar (DType TInt 0) x (show s)
                                        (_,b) <- lookupVar x
                                        return (ret, SDecrBool x b)
checkStmt s@(SRet e) ret          = do  t <- getReturnType
                                        ae <- checkExpr [t] e (show s)
                                        return (True, SRet ae)
checkStmt SVRet ret               = do  t <- getReturnType
                                        if t == TVoid
                                          then return ()
                                          else fail $ "Error in return: Expected TVoid, got " ++ (show t)
                                        return (ret, SVRet)
checkStmt (SIf e s) ret           = do  case e of
                                          ETrue -> do (ret', s') <- checkStmt s ret
                                                      return ( ret || ret', SIf (AExpr (DType TBool 0) e) s')
                                          _     -> do ae <- checkExpr [DType TBool 0] e ("Error in if condition, " ++ (show e))
                                                      (_, s') <- checkStmt s ret
                                                      return (ret, SIf ae s')
checkStmt (SIfElse e s1 s2) ret   = do  case e of
                                          ETrue -> do   (ret',s1') <- checkStmt s1 ret
                                                        (_,s2') <- checkStmt s2 ret
                                                        return (ret || ret', SIfElse (AExpr (DType TBool 0) e) s1' s2')
                                          EFalse -> do  (_,s1') <- checkStmt s1 ret
                                                        (ret',s2') <- checkStmt s2 ret
                                                        return (ret || ret', SIfElse (AExpr (DType TBool 0) e) s1' s2')
                                          _      -> do  ae <- checkExpr [DType TBool 0] e ("Error in if-else condition, " ++ (show e))
                                                        (ret',s1') <- checkStmt s1 ret
                                                        (ret'',s2') <- checkStmt s2 ret
                                                        return (ret || (ret' && ret''), SIfElse ae s1' s2')

checkStmt (SWhile e s) ret        = do  case e of
                                          ETrue -> do (ret',s') <- checkStmt s ret
                                                      return (ret || ret', SWhile (AExpr (DType TBool 0) e) s')
                                          _     -> do ae <- checkExpr [DType TBool 0] e ("Error in while condition, " ++ (show e))
                                                      (_,s') <- checkStmt s ret
                                                      return (ret, SWhile ae s')

checkStmt (SFor (DType t i) x e s) ret = 
  do ae <- checkExpr [DType t (i+1)] e ("Error in for loop, " ++ (show e))
     pushScope
     addVar (DType t i) x False
     (ret', s') <- checkStmt s ret
     popScope
     return (ret', (SFor (DType t i) x ae s'))
     
checkStmt s@(SFor t _ _ _) ret = fail $ (show s) ++ ": Error in for declaration: Only int, boolean or double (arrays) are allowed, got " ++ (show t)

inferExpr :: Expr -> S Expr
inferExpr exp = case exp of
  EId x           -> do (t,b) <- lookupVar x
                        return (AExpr t (EIdBool x b))

  EInteger i      -> return (AExpr (DType TInt 0) exp)

  EDouble d       -> return (AExpr (DType TDouble 0) exp)

  ETrue           -> return (AExpr (DType TBool 0) exp)

  EFalse          -> return (AExpr (DType TBool 0) exp)

  EApp fun es     -> do (ts, t) <- lookupFun fun
                        if (length ts) /= (length es)
                          then fail $ (show exp) ++ ": Wrong number of arguments"
                          else do aes <- mapM (\(t',e) -> checkExpr [t'] e (show exp)) (zip ts es)
                                  return (AExpr t (EAppTypes fun aes ts))

  EAppS fun str   -> do (ts, t) <- lookupFun fun
                        if head(ts) == TString then return (AExpr t exp) else fail $ (show exp) ++ ": Bad arguments"

  ENew (TIdent id) [] -> do t <- lookupStructClass id
                            return (AExpr t (ENew (TIdent id) []))

  ENew (DType t i) es -> do aes <- sequence [inferExpr e | e <- es]
                            let ts = and $ map (\(AExpr t _) -> t == (DType TInt 0)) aes
                            if ts == False
                              then fail $ (show exp) ++ " Indeces are not of type int"
                              else return (AExpr (DType t (i+(length es))) (ENew (DType t i) aes))

  EPtr e field    -> do ae@(AExpr t _) <- inferExpr e
                        case t of
                          TIdent i -> do t' <- lookupField field i
                                         return (AExpr t' (EPtr ae field))
                          _        -> fail $ (show exp) ++ ": Expected struct, got " ++ (show t)  

  ENull id       -> do  t <- lookupPointer id
                        return (AExpr t (ENull id))          
 
  EIndex e es      -> do ae@(AExpr t _) <- inferExpr e
                         case t of
                           (DType t' d) -> if d <= 0
                                             then fail $ (show e) ++ " is not an array"
                                             else do aes <- sequence [inferExpr e | e <- es ]
                                                     let ts =  and $ map (\(AExpr t _) -> t == (DType TInt 0))  aes
                                                     if d < (length es)
                                                       then fail $ (show exp) ++ " Too many indeces for array"
                                                       else if ts == False  
                                                         then fail $ (show exp) ++ " Indeces are not of type int"
                                                         else return (AExpr (DType t' (d-(length es))) (EIndex ae aes))
                           _            -> fail $ (show exp) ++ " is not an array"

  ESelf          -> do (b, i) <- getCheckingClass
                       case b of
                         True  -> return (AExpr (TIdent i) ESelf)
                         False -> fail $ "Can't use keyword self outside class methods"

  EDot e (EId "length") -> do ae@(AExpr (DType t d) _) <- inferExpr e
                              if d <= 0
                                then fail $ (show e) ++ " is not an array"
                                else return (AExpr (DType TInt 0) (EDot ae (EId "length")))

  EDot e1 (EApp mid es) -> 
    do ae1@(AExpr t' _) <- inferExpr e1
       case t' of
         TIdent cid -> do (ts, t) <- lookupMethod cid mid
                          if (length ts) /= (length es)
                            then fail $ (show exp) ++ ": Wrong number of arguments"
                            else do aes <- mapM (\(t'',e) -> checkExpr [t''] e (show exp)) (zip ts es)
                                    return (AExpr t (EDot ae1 (AExpr t (EAppTypes mid aes ts))))
         _          -> fail $ (show exp) ++ ": Invalid expression before ."


  EDot _ _              -> fail $ (show exp) ++ ": Invalid expression after ."

  ENeg e          -> do ae@(AExpr t _) <- checkExpr [DType TInt 0, DType TDouble 0] e (show exp)
                        return (AExpr t (ENeg ae))

  ENot e          -> do ae@(AExpr t _) <- checkExpr [DType TBool 0] e (show exp)
                        return (AExpr t (ENot ae))

  EMul e1 op e2   -> case op of
                       Mod -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0] (show exp)
                                 return (AExpr t (EMul ae1 op ae2))
                       Div -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0] (show exp)
                                 return (AExpr t (EMul ae1 op ae2))
                       Mul -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0] (show exp)
                                 return (AExpr t (EMul ae1 op ae2))

  EAdd e1 op e2   -> case op of
                       Plus  -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0] (show exp)
                                   return (AExpr t (EAdd ae1 op ae2))
                       Minus -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0] (show exp)
                                   return (AExpr t (EAdd ae1 op ae2))

  ERel e1 op e2   -> 
    do (AExpr e1t _) <- inferExpr e1
       case e1t of
         (TIdent id) -> case op of
                          Eq  -> do (t, ae1, ae2) <- inferBinOp e1 e2 [e1t] (show exp)
                                    return (AExpr (DType TBool 0) (ERel ae1 op ae2))
                          Neq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [e1t] (show exp)
                                    return (AExpr (DType TBool 0) (ERel ae1 op ae2))
                          _   -> fail $ (show exp) ++ ": Not a valid relational operator"
         _           -> case op of
                          Lth -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0] (show exp)
                                    return (AExpr (DType TBool 0) (ERel ae1 op ae2))
                          Leq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0] (show exp)
                                    return (AExpr (DType TBool 0) (ERel ae1 op ae2))
                          Gth -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0] (show exp)
                                    return (AExpr (DType TBool 0) (ERel ae1 op ae2))
                          Geq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0] (show exp)
                                    return (AExpr (DType TBool 0) (ERel ae1 op ae2))
                          Eq  -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0, DType TBool 0] (show exp)
                                    return (AExpr (DType TBool 0) (ERel ae1 op ae2))
                          Neq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TInt 0, DType TDouble 0, DType TBool 0] (show exp)
                                    return (AExpr (DType TBool 0) (ERel ae1 op ae2))

  EAnd e1 e2      -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TBool 0] (show exp)
                        return (AExpr t (EAnd ae1 ae2))
  EOr e1 e2       -> do (t, ae1, ae2) <- inferBinOp e1 e2 [DType TBool 0] (show exp)
                        return (AExpr t (EOr ae1 ae2))

checkVar :: DType -> Id -> ErrStr -> S ()
checkVar typ x err = do (typ2,b) <- lookupVar x
                        if typ == typ2 then return ()
                          else fail $ err ++ ": Expected " ++ show typ ++ ", got " ++ show typ2

checkExpr :: Types -> Expr -> ErrStr -> S Expr
checkExpr typs exp err = 
  do ae@(AExpr t _) <- inferExpr exp
     if elem t typs then return ae
                    else case t of
                      TIdent c -> do isClass <- checkIsClass c
                                     case isClass of
                                       True  -> do let t' = (head typs) -- Subtyping, we know that typs here always only contain just one type
                                                   case t' of
                                                     TIdent base -> do b <- checkIsParent base c
                                                                       case b of
                                                                         True  -> return ae
                                                                         False -> fail $ err ++ ": Class " ++ c ++ " is not extending any class " ++ base
                                                     _           -> fail $ err ++ ": Expected " ++ show typs ++ ", got " ++ show t
                                       False -> fail $ err ++ ": Expected " ++ show typs ++ ", got " ++ show t
                      _        -> fail $ err ++ ": Expected " ++ show typs ++ ", got " ++ show t

inferBinOp :: Expr -> Expr -> Types -> ErrStr -> S (DType, Expr, Expr)
inferBinOp e1 e2 typs err = do ae1@(AExpr t1 _) <- inferExpr e1
                               if elem t1 typs then
                                  do ae2 <- checkExpr [t1] e2 err
                                     return (t1, ae1, ae2)
                                     else fail $ err ++ ": Expected " ++ show typs ++ " for first operand, got " ++ show t1
