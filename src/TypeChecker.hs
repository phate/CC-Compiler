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
                      ([], TInt 0)  -> mapM checkDef defs
                      _             -> fail $ "Wrong definition of main function"

-- The predefined functions that can be used
predefFuns :: S ()
predefFuns = do addFun "printInt" ([TInt 0], TVoid)
                addFun "printDouble" ([TDouble 0], TVoid)
                addFun "printString" ([TString], TVoid)
                addFun "readInt" ([], TInt 0)
                addFun "readDouble" ([], TDouble 0)
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
     if retOk || (typ == TVoid) then return (FctDef typ id args (CStmt ss')) else fail $ "Function " ++ id ++ " does not return"
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
                                             Init id exp -> do ae <- checkExpr [typ] exp (show x)
                                                               addVar typ id
                                                               return (Init id ae)
-- FIXME: es is not checked and annotated
  SAss x es e         -> do typ <- lookupVar x
                            ae <- checkExpr [typ] e (show s)
                            return (ret, SAss x es ae)

  SExp e             -> do ae <- inferExpr e
                           return (ret, SExp ae)

  SIncr x            -> do checkVar (TInt 0) x (show s)
                           return (ret, SIncr x)

  SDecr x            -> do checkVar (TInt 0) x (show s)
                           return (ret, SDecr x)

  SRet e             -> do state <- get
                           let t1 = retType state
                           ae <- checkExpr [t1] e (show s)
                           return (True, SRet ae)

  SVRet              -> do state <- get
                           let t1 = retType state
                           if t1 == TVoid then return () else fail $ "Error in return: Expected TVoid, got " ++ (show t1)
                           return (ret, SVRet)

  SIf e stm          -> case e of
                          ETrue -> do (ret',s') <- checkStmt stm ret
                                      return (ret || ret', SIf (AExpr (TBool 0) e) s')
                          _     -> do ae <- checkExpr [TBool 0] e ("Error in if condition, " ++ (show e))
                                      (_,s') <- checkStmt stm ret
                                      return (ret, SIf ae s')

  SIfElse e s1 s2    -> case e of
                          ETrue  -> do (ret',s1') <- checkStmt s1 ret
                                       (_,s2') <- checkStmt s2 ret
                                       return (ret || ret', SIfElse (AExpr (TBool 0) e) s1' s2')
                          EFalse -> do (_,s1') <- checkStmt s1 ret
                                       (ret',s2') <- checkStmt s2 ret
                                       return (ret || ret', SIfElse (AExpr (TBool 0) e) s1' s2')
                          _      -> do ae <- checkExpr [TBool 0] e ("Error in if-else condition, " ++ (show e))
                                       (ret',s1') <- checkStmt s1 ret
                                       (ret'',s2') <- checkStmt s2 ret
                                       return (ret || (ret' && ret''), SIfElse ae s1' s2')

  SWhile e s         -> case e of
                          ETrue -> do (ret',s') <- checkStmt s ret
                                      return (ret || ret', SWhile (AExpr (TBool 0) e) s')
                          _     -> do ae <- checkExpr [TBool 0] e ("Error in while condition, " ++ (show e))
                                      (_,s') <- checkStmt s ret
                                      return (ret, SWhile ae s')
{----- END Statements -----}


{----- Expressions -----}
-- Infers an expression
inferExpr :: Expr -> S Expr
inferExpr exp = case exp of
  EId x           -> do t <- lookupVar x
                        return (AExpr t exp)

  EInteger i      -> return (AExpr (TInt 0) exp)

  EDouble d       -> return (AExpr (TDouble 0) exp)

  ETrue           -> return (AExpr (TBool 0) exp)

  EFalse          -> return (AExpr (TBool 0) exp)

  EApp fun es     -> do (ts, t) <- lookupFun fun
                        aes <- sequence [inferExpr e | e <- es ]
                        ts' <- sequence $ map (\(AExpr t _) -> return t) aes
                        if ts == ts' then return (AExpr t (EApp fun aes)) else fail $ (show exp) ++ ": Bad arguments"

  EAppS fun str   -> do (ts, t) <- lookupFun fun
                        if head(ts) == TString then return (AExpr t exp) else fail $ (show exp) ++ ": Bad arguments"

  ENew t es       -> do aes <- sequence [inferExpr e | e <- es]
                        let d = typeDim t
                        let ts = and $ map (\(AExpr t _) -> if t == (TInt 0) then True else False) aes
                        if ts == False
                          then fail $ (show exp) ++ " Indeces are not of type int"
                          else return (AExpr (setTypeDim t (d+(length es))) (ENew t es))                        
 
  EIdx id es      -> do t <- lookupVar id
                        let d = typeDim t
                        aes <- sequence [inferExpr e | e <- es ]
                        let ts =  and $ map (\(AExpr t _) -> if t == (TInt 0) then True else False)  aes
                        if d < (length es)
                          then fail $ (show exp) ++ " Too many indeces for array"
                          else if ts == False  
                                then fail $ (show exp) ++ " Indeces are not of type int"
                                else return (AExpr (setTypeDim t (d-(length es))) (EIdx id aes))  

  EDot x s        -> do t <- lookupVar x
                        if s /= "length"
                          then fail $ (show exp) ++ "Attribute not known"
                          else edot $ typeDim t
    where 
      edot i = if i <= 0
                then fail $ x ++ " is not an array"
                else return (AExpr (TInt 0) exp)

  ENeg e          -> do ae@(AExpr t _) <- checkExpr [TInt 0, TDouble 0] e (show exp)
                        return (AExpr t (ENeg ae))

  ENot e          -> do ae@(AExpr t _) <- checkExpr [TBool 0] e (show exp)
                        return (AExpr t (ENot ae))

  EMul e1 op e2   -> case op of
                       Mod -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0] (show exp)
                                 return (AExpr t (EMul ae1 op ae2))
                       Div -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0] (show exp)
                                 return (AExpr t (EMul ae1 op ae2))
                       Mul -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0] (show exp)
                                 return (AExpr t (EMul ae1 op ae2))

  EAdd e1 op e2   -> case op of
                       Plus  -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0] (show exp)
                                   return (AExpr t (EAdd ae1 op ae2))
                       Minus -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0] (show exp)
                                   return (AExpr t (EAdd ae1 op ae2))

  ERel e1 op e2   -> do case op of
                          Lth -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0] (show exp)
                                    return (AExpr (TBool 0) (ERel ae1 op ae2))
                          Leq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0] (show exp)
                                    return (AExpr (TBool 0) (ERel ae1 op ae2))
                          Gth -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0] (show exp)
                                    return (AExpr (TBool 0) (ERel ae1 op ae2))
                          Geq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0] (show exp)
                                    return (AExpr (TBool 0) (ERel ae1 op ae2))
                          Eq  -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0, TBool 0] (show exp)
                                    return (AExpr (TBool 0) (ERel ae1 op ae2))
                          Neq -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TInt 0, TDouble 0, TBool 0] (show exp)
                                    return (AExpr (TBool 0) (ERel ae1 op ae2))

  EAnd e1 e2      -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TBool 0] (show exp)
                        return (AExpr t (EAnd ae1 ae2))
  EOr e1 e2       -> do (t, ae1, ae2) <- inferBinOp e1 e2 [TBool 0] (show exp)
                        return (AExpr t (EOr ae1 ae2))

-- Checks whether a variable has the correct type
checkVar :: Type -> Id -> ErrStr -> S ()
checkVar typ x err = do typ2 <- lookupVar x
                        if typ == typ2 then return ()
                          else fail $ err ++ ": Expected " ++ show typ ++ ", got " ++ show typ2

-- Checks whether an expression has the correct type
checkExpr :: Types -> Expr -> ErrStr -> S Expr
checkExpr typs exp err = do ae@(AExpr t _) <- inferExpr exp
                            if elem t typs then return ae
                               else fail $ err ++ ": Expected " ++ show typs ++ ", got " ++ show t

-- Infers two expressions and checks whether they have the same and correct type
inferBinOp :: Expr -> Expr -> Types -> ErrStr -> S (Type, Expr, Expr)
inferBinOp e1 e2 typs err = do ae1@(AExpr t1 _) <- inferExpr e1
                               if elem t1 typs then
                                  do ae2 <- checkExpr [t1] e2 err
                                     return (t1, ae1, ae2)
                                     else fail $ err ++ ": Expected " ++ show typs ++ " for first operand, got " ++ show t1
{----- END Expressions -----}

typeDim :: Type -> Int
typeDim (TInt i) = i
typeDim (TDouble i) = i
typeDim (TBool i) = i

setTypeDim :: Type -> Int -> Type
setTypeDim (TInt _) i     = (TInt i)
setTypeDim (TDouble _) i  = (TDouble i)
setTypeDim (TBool _) i    = (TBool i)
