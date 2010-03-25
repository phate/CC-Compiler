module JVMGenerator where

import JVMEnv
import JVMAbs
import ParserAbs
import qualified Control.Monad.State


{----- Definitions -----}
generateInstructions :: Program -> JVMEnv
generateInstructions (Program defs) = Control.Monad.State.execState (generateDefs defs) newEnv

generateDefs :: [FctDef] -> S ()
generateDefs defs = mapM_ generateDef defs

generateDef :: FctDef -> S ()
generateDef (FctDef typ id args (CStmt ss)) = generateStmts ss


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
                            NoInit x   -> do i <- addVar x
                                             if typ == TDouble then put [DPush 0.0] else put [IPush 0]
                                             incStack (1)
                                             if typ == TDouble then put [DStore i] else put [IStore i]
                                             incStack (-1)
                            Init x exp -> do i <- addVar x
                                             generateExpr exp
                                             if typ == TDouble then put [DStore i] else put [IStore i]
                                             incStack (-1)

  SAss x (AExpr t e) -> do i <- lookupVar x
                           generateExpr (AExpr t e)
                           if t == TDouble then put [DStore i] else put [IStore i]
                           incStack (-1)

  SExp e             -> do generateExpr e
                           put [Pop] -- An SExp will always leave a value on the stack, so pop it
                           incStack (-1)

  SIncr x            -> do id <- lookupVar x
                           put [IInc id 1]

  SDecr x            -> do id <- lookupVar x
                           put [IInc id (-1)]

  SRet e             -> return () -- undefined i.e. todo
  SVRet              -> undefined -- todo
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
                        if t == TDouble then put [DLoad i] else put [ILoad i]
                        incStack (1)
                        put [Ifne label]
                        incStack (-1)

  ETrue           -> put [Goto label]

  EFalse          -> return ()

--  EApp fun es     -> undefined

--  ENot e          -> undefined

  ERel e1@(AExpr t1 _) op e2 -> 
    do generateExpr e1
       generateExpr e2
       case t1 of
         TInt    -> case op of
                      Lth -> do put [If_icmplt label]
                                incStack (-2)
                      Leq -> do put [If_icmple label]
                                incStack (-2)
                      Gth -> do put [If_icmpgt label]
                                incStack (-2)
                      Geq -> do put [If_icmpge label]
                                incStack (-2)
                      Eq  -> do put [If_icmpeq label]
                                incStack (-2)
                      Neq -> do put [If_icmpne label]
                                incStack (-2)
         TBool   -> case op of
                      Eq  -> do put [If_icmpeq label]
                                incStack (-2)
                      Neq -> do put [If_icmpne label]
                                incStack (-2)
         TDouble -> undefined -- todo

  EAnd e1 e2      -> do generateExpr e1
                        generateExpr e2
                        put [IAnd, Ifne label]
                        incStack (-2)

  EOr e1 e2       -> do generateExpr e1
                        generateExpr e2
                        put [IOr, Ifne label]
                        incStack (-2)

-- For expressions
generateExpr :: Expr -> S ()
generateExpr (AExpr t exp) = case exp of
  EId x           -> do i <- lookupVar x
                        if t == TDouble then put [DLoad i] else put [ILoad i]
                        incStack (1)

  EInteger i      -> do put [IPush i]
                        incStack (1)

  EDouble d       -> do put [DPush d]
                        incStack (1)

  ETrue           -> do put [IPush 1]
                        incStack (1)

  EFalse          -> do put [IPush 0]
                        incStack (1)

  EApp fun es     -> undefined -- todo

  EAppS fun str   -> undefined -- todo

  ENeg e          -> undefined -- todo

  ENot e          -> undefined -- todo

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
                                incStack (-1)
                      Mul -> do put [DMul]
                                incStack (-1)

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
                                  incStack (-1)
                      Minus -> do put [DSub]
                                  incStack (-1)

  ERel e1@(AExpr t1 _) op e2 -> 
    do trueLabel <- getLabel
       case t1 of
         TInt    -> case op of
                      Lth -> generateRel e1 e2 (If_icmplt trueLabel) trueLabel
                      Leq -> generateRel e1 e2 (If_icmple trueLabel) trueLabel
                      Gth -> generateRel e1 e2 (If_icmpgt trueLabel) trueLabel
                      Geq -> generateRel e1 e2 (If_icmpge trueLabel) trueLabel
                      Eq  -> generateRel e1 e2 (If_icmpeq trueLabel) trueLabel
                      Neq -> generateRel e1 e2 (If_icmplt trueLabel) trueLabel
         TDouble -> undefined -- todo

  EAnd e1 e2      -> do generateExpr e1
                        generateExpr e2
                        put [IAnd]
                        incStack (-1)

  EOr e1 e2       -> do generateExpr e1
                        generateExpr e2
                        put [IOr]
                        incStack (-1)


generateRel :: Expr -> Expr -> JVMInstr -> LabelStr -> S ()
generateRel e1 e2 i trueLabel = do endLabel <- getLabel
                                   generateExpr e1
                                   generateExpr e2
                                   put [i, IPush 0, Goto endLabel, Label trueLabel, IPush 1, Label endLabel]
{----- END Expressions -----}
