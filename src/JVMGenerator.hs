module JVMGenerator where

import JVMEnv
import JVMAbs
import ParserAbs
import qualified Control.Monad.State


{----- Definitions -----}
generateInstructions :: Program -> [JVMEnv]
generateInstructions (Program defs) = generateDefs defs

generateDefs :: [FctDef] -> [JVMEnv]
generateDefs []     = []
generateDefs (x:xs) = (Control.Monad.State.execState (generateDef x) newEnv):(generateDefs xs)

generateDef :: FctDef -> S ()
generateDef (FctDef typ id args (CStmt ss)) = 
  do setFunctionName id
     setParameters [ t | (Arg t _) <- args]
     setReturnType typ
     mapM_ (\(x, t) -> addVar x t) [(id', t) | (Arg t id') <- args]
     generateStmts ss
     case typ of -- Always add an ending return (fixes: "falling of code" for void-functions, and "illegal target of jump or branch")
       TVoid   -> put [VReturn]
       TDouble -> put [DReturn]
       _       -> put [IReturn]


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

  SDecl typ vars     -> mapM_ checkDecl vars where
                          checkDecl :: Item -> S ()
                          checkDecl var = case var of
                            NoInit x   -> do i <- addVar x typ
                                             case typ of
                                               TDouble -> do put [DPush 0.0]
                                                             incStack (2)
                                                             put [DStore i]
                                                             incStack (-2)
                                               _       -> do put [IPush 0]
                                                             incStack (1)
                                                             put [IStore i]
                                                             incStack (-1)
                            Init x exp -> do generateExpr exp
                                             i <- addVar x typ
                                             case typ of
                                               TDouble -> do put [DStore i]
                                                             incStack (-2)
                                               _       -> do put [IStore i]
                                                             incStack (-1)

  SAss x (AExpr t e) -> do i <- lookupVar x
                           generateExpr (AExpr t e)
                           case t of
                             TDouble -> do put [DStore i]
                                           incStack (-2)
                             _       -> do put [IStore i]
                                           incStack (-1)

  SExp e             -> do generateExpr e
                           case e of
                             (AExpr TVoid (EApp _ _))  -> return ()
                             (AExpr TVoid (EAppS _ _)) -> return ()
                             (AExpr TDouble _)         -> do put [Pop2]
                                                             incStack (-2)
                             _                         -> do put [Pop]
                                                             incStack (-1)

  SIncr x            -> do id <- lookupVar x
                           put [IInc id 1]

  SDecr x            -> do id <- lookupVar x
                           put [IInc id (-1)]

  SRet e             -> do generateExpr e
                           ret <- getReturnType
                           case ret of
                             TDouble -> do put [DReturn]
                                           incStack (-2)
                             _       -> do put [IReturn]
                                           incStack (-1)
                           
  SVRet              -> put [VReturn]
  
  SIf e s            -> do stmLabel <- getLabel
                           endLabel <- getLabel
                           genTestExpr e stmLabel
                           put [Goto endLabel, Label stmLabel]
                           generateStmt s
                           put [Label endLabel]
                           
  SIfElse e s1 s2    -> do s1Label <- getLabel
                           endLabel <- getLabel
                           genTestExpr e s1Label
                           generateStmt s2
                           put [Goto endLabel, Label s1Label]
                           generateStmt s1
                           put [Label endLabel]
                           
  SWhile e s         -> do stmLabel <- getLabel
                           testLabel <- getLabel
                           put [Goto testLabel, Label stmLabel]
                           generateStmt s
                           put [Label testLabel]
                           genTestExpr e stmLabel
                           
{----- END Statements -----}


{----- Expressions -----}

-- For expressions in control structures that returns bool, i.e. in while, if, if-else
-- Branch to label if the expression is true.
genTestExpr :: Expr -> LabelStr -> S ()
genTestExpr (AExpr t exp) label = case exp of
  EId x           -> do i <- lookupVar x
                        put [ILoad i]
                        incStack (1)
                        put [Ifne label]
                        incStack (-1)

  ETrue           -> put [Goto label]

  EFalse          -> return ()

  EApp fun es     -> do mapM_ generateExpr es
                        let types = [ t' | (AExpr t' _) <- es]
                        put [InvokeStatic fun types t]
                        incStack $ sum [if x == TDouble then (-2) else (-1) | x <- types]
                        incStack(1)
                        put [Ifne label]
                        incStack (-1)

  ENot e          -> do generateExpr e
                        put [IPush 1]
                        incStack (1)
                        put [IXor]
                        put [Ifne label]
                        incStack (-1)

  ERel e1@(AExpr t1 _) op e2 -> 
    do generateExpr e1
       generateExpr e2
       case t1 of
         TInt    -> do incStack (-2)
                       case op of
                         Lth -> put [If_icmplt label]
                         Leq -> put [If_icmple label]
                         Gth -> put [If_icmpgt label]
                         Geq -> put [If_icmpge label]
                         Eq  -> put [If_icmpeq label]
                         Neq -> put [If_icmpne label]
                         
         TBool   -> do incStack (-2)
                       case op of
                         Eq  -> put [If_icmpeq label]
                         Neq -> put [If_icmpne label]
                         
         TDouble -> do put [Dcmpl] -- Dcmpl returns: 1: e1>e2, 0: e1=e2, -1: e1<e2 or NaN
                       incStack (-4)
                       case op of
                         Lth -> put [Iflt label]
                         Leq -> put [Ifle label]
                         Gth -> put [Ifgt label]
                         Geq -> put [Ifge label]
                         Eq  -> put [Ifeq label]
                         Neq -> put [Ifne label]


  EAnd e1 e2      -> do falseLabel <- getLabel
                        generateExpr e1
                        put [Ifeq falseLabel]
                        incStack (-1)
                        generateExpr e2
                        put [Ifne label]
                        incStack (-1)
                        put [Label falseLabel]

  EOr e1 e2       -> do generateExpr e1
                        put [Ifne label]
                        incStack (-1)
                        generateExpr e2
                        put [Ifne label]
                        incStack (-1)

-- For expressions
generateExpr :: Expr -> S ()
generateExpr (AExpr t exp) = case exp of
  EId x           -> do i <- lookupVar x
                        case t of
                          TDouble -> do put [DLoad i]
                                        incStack (2)
                          _       -> do put [ILoad i]
                                        incStack (1)

  EInteger i      -> do put [IPush i]
                        incStack (1)

  EDouble d       -> do put [DPush d]
                        incStack (2)

  ETrue           -> do put [IPush 1]
                        incStack (1)

  EFalse          -> do put [IPush 0]
                        incStack (1)

  EApp fun es     -> do mapM_ generateExpr es
                        let types = [ t' | (AExpr t' _) <- es]
                        put [InvokeStatic fun types t]
                        incStack $ sum [if x == TDouble then (-2) else (-1) | x <- types]
                        case t of
                          TVoid -> return ()
                          TDouble -> incStack (2)
                          _       -> incStack (1)

  EAppS fun str   -> do put [SPush str]
                        incStack (1)
                        put [InvokeStatic fun [TString] t]
                        incStack (-1)

  ENeg e@(AExpr t' _) -> do generateExpr e
                            case t' of
                              TInt    -> put [INeg]
                              TDouble -> put [DNeg]

  ENot e          -> do generateExpr e
                        put [IPush 1]
                        incStack (1)
                        put [IXor]
                        incStack (-1)

  EMul e1@(AExpr t1 _) op e2 -> 
    do generateExpr e1
       generateExpr e2
       case t1 of
         TInt    -> case op of
                      Mod -> do put [IRem]
                                incStack (-1)
                      Div -> do put [IDiv]
                                incStack (-1)
                      Mul -> do put [IMul]
                                incStack (-1)
         TDouble -> case op of
                      Div -> do put [DDiv]
                                incStack (-2)
                      Mul -> do put [DMul]
                                incStack (-2)

  EAdd e1@(AExpr t1 _) op e2 -> 
    do generateExpr e1
       generateExpr e2
       case t1 of
         TInt    -> case op of
                      Plus  -> do put [IAdd]
                                  incStack (-1)
                      Minus -> do put [ISub]
                                  incStack (-1)
         TDouble -> case op of
                      Plus  -> do put [DAdd]
                                  incStack (-2)
                      Minus -> do put [DSub]
                                  incStack (-2)

  ERel e1@(AExpr t1 _) op e2 -> 
    do trueLabel <- getLabel
       case t1 of
         TInt    -> case op of
                      Lth -> generateRel e1 e2 (If_icmplt trueLabel) trueLabel
                      Leq -> generateRel e1 e2 (If_icmple trueLabel) trueLabel
                      Gth -> generateRel e1 e2 (If_icmpgt trueLabel) trueLabel
                      Geq -> generateRel e1 e2 (If_icmpge trueLabel) trueLabel
                      Eq  -> generateRel e1 e2 (If_icmpeq trueLabel) trueLabel
                      Neq -> generateRel e1 e2 (If_icmpne trueLabel) trueLabel
         TBool   -> case op of
                      Eq  -> generateRel e1 e2 (If_icmpeq trueLabel) trueLabel
                      Neq -> generateRel e1 e2 (If_icmpne trueLabel) trueLabel
         TDouble -> case op of
                      Lth -> generateRelDouble e1 e2 (Iflt trueLabel) trueLabel -- Dcmpl returns: 1: e1>e2, 0: e1=e2, -1: e1<e2 or NaN
                      Leq -> generateRelDouble e1 e2 (Ifle trueLabel) trueLabel
                      Gth -> generateRelDouble e1 e2 (Ifgt trueLabel) trueLabel
                      Geq -> generateRelDouble e1 e2 (Ifge trueLabel) trueLabel
                      Eq  -> generateRelDouble e1 e2 (Ifeq trueLabel) trueLabel
                      Neq -> generateRelDouble e1 e2 (Ifne trueLabel) trueLabel
                                

  EAnd e1 e2      -> do falseLabel <- getLabel
                        put [IPush 0]
                        incStack(1)
                        generateExpr e1
                        put [Ifeq falseLabel]
                        incStack (-1)
                        put [Pop]
                        incStack (-1)
                        generateExpr e2
                        put [Label falseLabel]

  EOr e1 e2       -> do trueLabel <- getLabel
                        put [IPush 1]
                        incStack(1)
                        generateExpr e1
                        put [Ifne trueLabel]
                        incStack (-1)
                        put [Pop]
                        incStack (-1)
                        generateExpr e2
                        put [Label trueLabel]


generateRel :: Expr -> Expr -> JVMInstr -> LabelStr -> S ()
generateRel e1 e2 i trueLabel = do endLabel <- getLabel
                                   generateExpr e1
                                   generateExpr e2
                                   put [i]
                                   incStack (-2)
                                   put [IPush 0, Goto endLabel, Label trueLabel, IPush 1, Label endLabel]
                                   incStack (1)
                                   
generateRelDouble :: Expr -> Expr -> JVMInstr -> LabelStr -> S ()
generateRelDouble e1 e2 i trueLabel = do endLabel <- getLabel
                                         generateExpr e1
                                         generateExpr e2
                                         put [Dcmpl]
                                         incStack (-3)
                                         put [i]
                                         incStack (-1)
                                         put [IPush 0, Goto endLabel, Label trueLabel, IPush 1, Label endLabel]
                                         incStack (1)
{----- END Expressions -----}
