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
     setFunctionReturnType typ
     setFunctionParameters [(t, id') | (Arg t id') <- args]
     generateStmts ss
     case typ of
       TVoid -> do add VRet
                   fixLabels
       _     -> fixLabels
     return ()


{----- END Definitions -----}


{----- Statements -----}
generateStmts :: [Stmt] -> S ()
generateStmts ss = mapM_ generateStmt ss 

generateStmt :: Stmt -> S ()
generateStmt s = case s of
  SEmpty             -> return ()

  SCStmt (CStmt ss)  -> do pushScope
                           generateStmts ss
                           popScope

  SDecl typ vars     -> 
    mapM_ checkDecl vars where
      checkDecl :: Item -> S ()
      checkDecl var = case var of
        NoInit x   -> do ptr <- addVar x
                         case typ of
                           TInt    -> add (IAlloca ptr)
                           TBool   -> add (BAlloca ptr)
                           TDouble -> add (DAlloca ptr)
                         return ()
        Init x exp -> do val <- generateExpr exp
                         ptr <- addVar x
                         case typ of
                           TInt    -> do add (IAlloca ptr) 
                                         add (IStore val ptr)
                           TBool   -> do add (BAlloca ptr)
                                         add (BStore val ptr)
                           TDouble -> do add (DAlloca ptr)
                                         add (DStore val ptr)
                         return ()

  SAss x exp@(AExpr t e) -> 
    do ptr <- lookupVar x
       val <- generateExpr exp
       case t of
         TInt    -> add (IStore val ptr)
         TBool   -> add (IStore val ptr)
         TDouble -> add (DStore val ptr)

  SExp e             -> do generateExpr e
                           return ()

  SIncr x            -> do ptr <- lookupVar x
                           destReg <- getRegister
                           addReg <- getRegister
                           add (ILoad destReg ptr)
                           add (IAdd addReg (VReg destReg) (VInt 1))
                           add (IStore (VReg addReg) ptr)
                           return ()

  SDecr x            -> do ptr <- lookupVar x
                           destReg <- getRegister
                           addReg <- getRegister
                           add (ILoad destReg ptr)
                           add (ISub addReg (VReg destReg) (VInt 1))
                           add (IStore (VReg addReg) ptr)
                           return () 
  
  SRet e             -> do v <- generateExpr e
                           ret <- getFunctionReturnType
                           case ret of
                             TDouble -> add (DRet v)
                             TInt    -> add (IRet v)
                             TBool   -> add (BRet v)

  SVRet              -> add (VRet)

  SIf e s            -> do trueLabel <- getLabel
                           falseLabel <- getLabel
                           val <- generateExpr e
                           add (Br val trueLabel falseLabel)
                           add (Label trueLabel)
                           generateStmt s
                           add (UBr falseLabel)
                           add (Label falseLabel)

  SIfElse e s1 s2    -> do trueLabel <- getLabel
                           falseLabel <- getLabel
                           endLabel <- getLabel
                           val <- generateExpr e
                           add (Br val trueLabel falseLabel)
                           add (Label trueLabel)
                           generateStmt s1
                           add (UBr endLabel)
                           add (Label falseLabel)
                           generateStmt s2
                           add (UBr endLabel)
                           add (Label endLabel)

  SWhile e s         -> do testLabel <- getLabel
                           trueLabel <- getLabel
                           falseLabel <- getLabel
                           add (UBr testLabel)
                           add (Label testLabel)
                           val <- generateExpr e
                           add (Br val trueLabel falseLabel)
                           add (Label trueLabel)
                           generateStmt s
                           add (UBr testLabel)
                           add (Label falseLabel)
                           
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

  EId x           -> do ptr <- lookupVar x
                        destReg <- getRegister
                        case t of
                          TInt    -> add (ILoad destReg ptr)
                          TBool   -> add (BLoad destReg ptr)
                          TDouble -> add (DLoad destReg ptr)
                        return (VReg destReg)
  EInteger i      -> return (VInt i)
  EDouble d       -> return (VDouble d)
  
  ETrue           -> return (VBool 1)
  EFalse          -> return (VBool 0)

  ENeg e@(AExpr t' _) -> undefined
  ENot e          -> undefined
  EAnd e1 e2      -> undefined
  EOr e1 e2       -> undefined
  
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
         TDouble -> do destReg <- getRegister
                       add (DCall destReg fun args)
                       return (VReg destReg)
         TVoid   -> do add (VCall fun args)
                       return VUndef

  EAppS fun str -> do destReg <- getRegister
                      g <- addGlobal str
                      add (GetElemPtr destReg (length str) g)
                      add (VCall fun [(TString, (VReg destReg))])
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
