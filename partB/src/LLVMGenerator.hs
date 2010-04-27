module LLVMGenerator where

import LLVMEnv
import LLVMAbs
import ParserAbs
import qualified Control.Monad.State


{----- Definitions -----}
generateInstructions :: Program -> [LLVMEnv]
generateInstructions (Program defs) = generateDefs defs

generateDefs :: [FctDef] -> [LLVMEnv]
generateDefs []     = []
generateDefs (x:xs) = (Control.Monad.State.execState (generateDef x) newEnvironment):(generateDefs xs)

generateDef :: FctDef -> S ()
generateDef (FctDef typ id args (CStmt ss)) = 
  do setFunctionName id
     setFunctionParameters [ (t, i) | (Arg t i) <- args]
     setFunctionReturnType typ
     mapM_ (\(x, t) -> addVar x t) [(id', t) | (Arg t id') <- args]
     generateStmts ss
     return ()


{----- END Definitions -----}


{----- Statements -----}
generateStmts :: [Stmt] -> S ()
generateStmts ss = mapM_ generateStmt ss 

generateStmt :: Stmt -> S ()
generateStmt s = case s of
  SEmpty             -> return ()
  SExp e             -> do generateExpr e
                           return ()
  
  SRet e             -> do v <- generateExpr e
                           ret <- getFunctionReturnType
                           case ret of
                             TDouble -> add (IRet v) -- Should be DRet in the future
                             TInt    -> add (IRet v)
                             TBool   -> add (BRet v)
  _                  -> return ()
                           
{----- END Statements -----}


{----- Expressions -----}

-- For expressions in control structures that returns bool, i.e. in while, if, if-else
-- Branch to label if the expression is true.
generateTestExpr :: Expr -> String -> S ()
generateTestExpr (AExpr t exp) label = case exp of
  _               -> return ()


-- For expressions
generateExpr :: Expr -> S Value
generateExpr (AExpr t exp) = case exp of

  EInteger i      -> return (VInt i)
  EDouble d       -> return (VDouble d)
  
  ETrue           -> return (VBool 1)
  EFalse          -> return (VBool 0)
  
  EAdd e1 op e2 -> 
    do val1 <- generateExpr e1
       val2 <- generateExpr e2
       destReg <- getRegister
       case t of
         TInt    -> case op of
                      Plus  -> add (IAdd destReg val1 val2)
                      Minus -> add (ISub destReg val1 val2)
         TDouble -> case op of
                      Plus  -> add (DAdd destReg val1 val2)
                      Minus -> add (DSub destReg val1 val2)
       return (VReg destReg)
       
  EMul e1 op e2 ->
    do val1 <- generateExpr e1
       val2 <- generateExpr e2
       destReg <- getRegister
       case t of
         TInt    -> case op of
                      Mul -> add (IMul destReg val1 val2)
                      Div -> add (IDiv destReg val1 val2)
                      Mod -> add (IRem destReg val1 val2)
         TDouble -> case op of
                      Mul -> add (DMul destReg val1 val2)
                      Div -> add (DDiv destReg val1 val2)
       return (VReg destReg)
       
  EApp fun es -> 
    do values <- mapM generateExpr es
       let args = zip [ t' | (AExpr t' _) <- es] values
       case t of
         TInt    -> do destReg <- getRegister
                       add (ICall destReg fun args)
                       return (VReg destReg)
         TBool   -> do destReg <- getRegister
                       add (BCall destReg fun args)
                       return (VReg destReg)
         TVoid   -> do add (VCall fun args)
                       return VUndef
                     
  ERel e1@(AExpr t1 _) op e2 -> 
    do val1 <- generateExpr e1
       val2 <- generateExpr e2
       destReg <- getRegister
       case t1 of
         TInt    -> case op of
                      Lth -> add (ILt destReg val1 val2)
                      Leq -> add (ILe destReg val1 val2)
                      Gth -> add (IGt destReg val1 val2)
                      Geq -> add (IGe destReg val1 val2)
                      Eq  -> add (IEq destReg val1 val2)
                      Neq -> add (INe destReg val1 val2)
         TBool   -> case op of
                      Eq  -> add (BEq destReg val1 val2)
                      Neq -> add (BNe destReg val1 val2)
         TDouble -> case op of
                      Lth -> add (DLt destReg val1 val2)
                      Leq -> add (DLe destReg val1 val2)
                      Gth -> add (DGt destReg val1 val2)
                      Geq -> add (DGe destReg val1 val2)
                      Eq  -> add (DEq destReg val1 val2)
                      Neq -> add (DNe destReg val1 val2)
       return (VReg destReg)


{----- END Expressions -----}
