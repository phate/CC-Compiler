module TypeChecker where

import TypeCheckerEnv
import ParserAbs
import Control.Monad.State
import ErrM

-- Typechecks the Program
typecheck :: Program -> Err (Program, EnvState)
typecheck (Program defs) = do (defs', env) <- runStateT (checkDefs defs) newEnv
                              return ((Program defs'), env)

{----- Definitions -----}
-- Typechecks all top definitions
checkDefs :: [FctDef] -> S [FctDef]
checkDefs defs = do predefFuns
                    addDefs defs
                    mainFun <- lookupFun "main"
                    case mainFun of
                      ([], TInt)  -> mapM checkDef defs
                      _           -> fail $ "Wrong main function"

-- The predefined functions that can be used
predefFuns :: S ()
predefFuns = do addFun "printInt" ([TInt], TVoid)
                addFun "printDouble" ([TDouble], TVoid)
                addFun "printString" ([TString], TVoid)
                addFun "readInt" ([], TInt)
                addFun "readDouble" ([], TDouble)
                return ()

-- Adds all top definitions to the environment, i.e. creates the signature
addDefs :: [FctDef] -> S ()
addDefs defs = mapM_ (\(FctDef typ id args _) -> addFun id ([t | (Arg t id) <- args], typ)) defs

-- Typechecks one top definition
checkDef :: FctDef -> S FctDef
checkDef (FctDef typ id args (CStmt ss)) = 
  do emptyContext
     setReturnType typ
     mapM_ (\(t,x) -> addVar t x) [(t',id') | (Arg t' id') <- args]
     (retOk,ss') <- checkStmts ss False
     if retOk || (typ == TVoid) then return (FctDef typ id args (CStmt ss')) else fail $ "NoRetError"
{----- END Definitions -----}


{----- Statements -----}
-- Typechecks all statements in a statement list
-- (The returned boolean is used for checking if the function returns)
checkStmts :: [Stmt] -> Bool -> S (Bool,[Stmt])
checkStmts [] ret     = return (ret,[])
checkStmts (s:ss) ret = do (ret', s') <- checkStmt s ret
                           (ret'',ss') <- checkStmts ss (ret || ret')
                           return (ret'', s':ss')	

-- Typechecks one statement
checkStmt :: Stmt -> Bool -> S (Bool,Stmt)
checkStmt s ret = case s of
  SEmpty             -> return (ret,SEmpty)
  SCStmt (CStmt ss)  -> do pushScope
                           (ret', ss') <- checkStmts ss ret
                           popScope
                           return (ret || ret', SCStmt (CStmt ss'))

  SDecl typ vars     -> do vars' <- mapM checkDecl vars 
                           return (ret,(SDecl typ vars')) where
                             checkDecl :: Item -> S Item
                             checkDecl x = case x of
                                             NoInit id   -> do addVar typ id
                                                               return (NoInit id)
                                             Init id exp -> do ae <- checkExpr [typ] exp "DeclErr"
                                                               addVar typ id
                                                               return (Init id ae)

  SAss x e           -> do typ <- lookupVar x
                           ae <- checkExpr [typ] e "x = expr "
                           return (ret, SAss x ae)

  SExp e             -> do ae <- inferExpr e
                           return (ret, SExp ae)

  SIncr x            -> do checkVar TInt x "i++"
                           return (ret, SIncr x)

  SDecr x            -> do checkVar TInt x "i--"
                           return (ret, SDecr x)

  SRet e             -> do state <- get
                           let t1 = retType state
                           ae <- checkExpr [t1] e "ReturnFail"
                           return (True, SRet ae)

  SVRet              -> do state <- get
                           let t1 = retType state
                           if t1 == TVoid then return () else fail $ "VoidReturnFail"
                           return (ret, SVRet)

  SIf e s            -> case e of
                          ETrue -> do (ret',s') <- checkStmt s ret
                                      return (ret || ret', SIf (AExpr TBool e) s')
                          _     -> do ae <- checkExpr [TBool] e "IfNotBool"
                                      (_,s') <- checkStmt s ret
                                      return (ret, SIf ae s')

  SIfElse e s1 s2    -> case e of
                          ETrue  -> do (ret',s1') <- checkStmt s1 ret
                                       (_,s2') <- checkStmt s2 ret
                                       return (ret || ret', SIfElse (AExpr TBool e) s1' s2')
                          EFalse -> do (_,s1') <- checkStmt s1 ret
                                       (ret',s2') <- checkStmt s2 ret
                                       return (ret || ret', SIfElse (AExpr TBool e) s1' s2')
                          _      -> do ae <- checkExpr [TBool] e "IfElseNotBool"
                                       (ret',s1') <- checkStmt s1 ret
                                       (ret'',s2') <- checkStmt s2 ret
                                       return (ret || (ret' && ret''), SIfElse ae s1' s2')

  SWhile e s         -> case e of
                          ETrue -> do (ret',s') <- checkStmt s ret
                                      return (ret || ret', SWhile (AExpr TBool e) s')
                          _     -> do ae <- checkExpr [TBool] e "WhileNotBool"
                                      (_,s') <- checkStmt s ret
                                      return (ret, SWhile ae s')
{----- END Statements -----}


{----- Expressions -----}
-- Infers an expression
inferExpr :: Expr -> S Expr
inferExpr exp = case exp of
  EId x  	  -> do t <- lookupVar x
                        return (AExpr t exp)

  EInteger i      -> return (AExpr TInt exp)

  EDouble d       -> return (AExpr TDouble exp)

  ETrue           -> return (AExpr TBool exp)

  EFalse          -> return (AExpr TBool exp)

  EApp fun es     -> do (ts, t) <- lookupFun fun
                        aes <- sequence [inferExpr e | e <- es ]
                        ts' <- sequence $ map (\(AExpr t _) -> return t) aes
                        if ts == ts' then return (AExpr t (EApp fun aes)) else fail $ "FunApp"

  EAppS fun str   -> do (ts, t) <- lookupFun fun
                        if head(ts) == TString then return (AExpr t exp) else fail $ "FunAppS"

  ENeg e          -> do ae@(AExpr t _) <- checkExpr [TInt, TDouble] e "NegFail"
                        return (AExpr t (ENeg ae))

  ENot e          -> do ae@(AExpr t _) <- checkExpr [TBool] e "NotFail"
                        return (AExpr t (ENot ae))

  EMul e1 op e2   -> case op of
                       Mod -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt] "%"
                                 return (AExpr t (EMul ae1 op ae2))
                       Div -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble] "/"
                                 return (AExpr t (EMul ae1 op ae2))
                       Mul -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble] "*"
                                 return (AExpr t (EMul ae1 op ae2))

  EAdd e1 op e2   -> case op of
                       Plus  -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble] "+"
                                   return (AExpr t (EAdd ae1 op ae2))
                       Minus -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble] "-"
                                   return (AExpr t (EAdd ae1 op ae2))

  ERel e1 op e2   -> do case op of
                          Lth -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble] "<"
                                    return (AExpr TBool (ERel ae1 op ae2))
                          Leq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble] "<="
                                    return (AExpr TBool (ERel ae1 op ae2))
                          Gth -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble] ">"
                                    return (AExpr TBool (ERel ae1 op ae2))
                          Geq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble] ">="
                                    return (AExpr TBool (ERel ae1 op ae2))
                          Eq  -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble, TBool] "=="
                                    return (AExpr TBool (ERel ae1 op ae2))
                          Neq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt, TDouble, TBool] "!="
                                    return (AExpr TBool (ERel ae1 op ae2))

  EAnd e1 e2      -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TBool] "&&"
                        return (AExpr t (EAnd ae1 ae2))
  EOr e1 e2       -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TBool] "||"
                        return (AExpr t (EOr ae1 ae2))

-- Checks whether a variable has the correct type
checkVar :: Type -> Id -> ErrStr -> S ()
checkVar typ x err = do typ2 <- lookupVar x
                        if typ == typ2 then return ()
                           else fail $ err ++ show typ2

-- Checks whether an expression has the correct type
checkExpr :: Types -> Expr -> ErrStr -> S Expr
checkExpr typs exp err = do ae@(AExpr t _) <- inferExpr exp
                            if elem t typs then return ae
                               else fail $ err ++ show t

-- Infers two expressions and checks whether they have the same and correct type
inferBinOp :: Expr -> Expr -> Types -> ErrStr -> S (Type, Expr, Expr)
inferBinOp e1 e2 typs err = do ae1@(AExpr t1 _) <- inferExpr e1
                               if elem t1 typs then
                                  do ae2 <- checkExpr [t1] e2 err
                                     return (t1, ae1, ae2)
                                     else fail $ err
{----- END Expressions -----}
