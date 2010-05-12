module LLVMPrinter where

import LLVMAbs
import LLVMEnv
import ParserAbs(DType(..),Type(..))

getCode :: LLVMEnv -> String
getCode env = printHeader env ++ (unlines $ [printFunction e | e <- fctEnv env ])

printHeader :: LLVMEnv -> String
printHeader env = printPreDefFun ++ 
                  (printGlobStrings $ globStrings env)

printPreDefFun :: String
printPreDefFun =  "\n\ndeclare void @printInt(i32)\n" ++
                  "declare void @printDouble(double)\n" ++ 
                  "declare void @printString(i8*)\n" ++
                  "declare i32 @readInt()\n" ++
                  "declare double @readDouble()\n\n" 

printGlobStrings ::[(LLVMId, String)] -> String
printGlobStrings = concatMap  (\(id, s) -> "@" ++ id ++ " = internal constant [ " ++
                              (show $ (length s)+1) ++ " x i8] c\"" ++ s ++ "\\00\"\n")

printFunction :: FctEnv -> String
printFunction env = "define " ++ (printDType $ returnType env) ++
                    " @" ++ funName env ++
                    "( " ++ sepArgs args ++
                    " ){" ++
                    (unlines $ printInstructions $ instr env) ++ "}\n"
  where args = zipWith (\(t,id) i -> printDType t ++ " " ++ (printOp (OId $ createPLLVMId $ id2LLVMId id i))) (params env) [0..]
 
sepArgs :: [String] -> String
sepArgs s = sepArgs' s []
  where sepArgs' [] _ = []
        sepArgs' (x:[]) ss = concat $ reverse $ x:ss
        sepArgs' (x:xs) ss = sepArgs' xs ((x++", "):ss)

printInstructions :: [LLVMInstr] -> [String]
printInstructions instr = undefined

{-

printInstructions :: [LLVMInstr] -> [String]
printInstructions instr = map printInstruction (reverse instr)

printInstruction :: LLVMInstr -> String
printInstruction (LLAdd tr (TInt 0) v1 v2) = printBinOp (tr,(TInt 0),v1,v2) "add" ""
printInstruction (LLAdd tr (TDouble 0) v1 v2) = printBinOp (tr,(TDouble 0),v1,v2) "fadd" ""
printInstruction (LLSub tr (TInt 0) v1 v2) = printBinOp (tr,(TInt 0),v1,v2) "sub" ""
printInstruction (LLSub tr (TDouble 0) v1 v2) = printBinOp (tr,(TDouble 0),v1,v2) "fsub" ""
printInstruction (LLMul tr (TInt 0) v1 v2) = printBinOp (tr,(TInt 0),v1,v2) "mul" ""
printInstruction (LLMul tr (TDouble 0) v1 v2) = printBinOp (tr,(TDouble 0),v1,v2) "fmul" ""
printInstruction (LLDiv tr (TInt 0) v1 v2) = printBinOp (tr,(TInt 0),v1,v2) "sdiv" ""
printInstruction (LLDiv tr (TDouble 0) v1 v2) = printBinOp (tr,(TDouble 0),v1,v2) "fdiv" ""
printInstruction (LLRem tr (TInt 0) v1 v2) = printBinOp (tr,(TInt 0),v1,v2) "srem" ""
printInstruction (LLRem tr (TDouble 0) v1 v2) = printBinOp (tr,(TDouble 0),v1,v2) "frem" ""
printInstruction (LLAnd tr v1 v2) = printBinOp (tr,(TBool 0),v1,v2) "and" ""
printInstruction (LLOr tr v1 v2) = printBinOp (tr,(TBool 0),v1,v2) "or" ""
printInstruction (LLXor tr v1 v2) = printBinOp (tr,(TBool 0),v1,v2) "xor" ""
printInstruction (LLCmp tr (TInt 0) pf v1 v2) = printBinOp (tr,(TInt 0),v1,v2) "icmp" pf
printInstruction (LLCmp tr (TBool 0) pf v1 v2) = printBinOp (tr,(TBool 0),v1,v2) "icmp" pf
printInstruction (LLCmp tr (TDouble 0) pf v1 v2) = printBinOp (tr,(TDouble 0),v1,v2) "fcmp" pf
printInstruction (LLLoad tr t lid) = (printOp tr) ++ " = " ++ "load " ++ (printType t) ++ "*" ++ " %" ++ lid
printInstruction (LLStore t v1 v2) = "store " ++ (printType t) ++ " " ++ (printOp v1) ++ ", " ++ (printType t) ++ "* " ++ (printOp v2)
printInstruction (LLReturn t v) = "ret " ++ (printType t) ++ " " ++ (printOp v)
printInstruction LLVReturn = "ret void"
printInstruction (LLAlloc tr t) = (printOp tr) ++ " = " ++ "alloca " ++ (printType t)
printInstruction (LLBr l) = "br label " ++ "%" ++ l
printInstruction (LLCBr v ltrue lfalse) = "br i1 " ++ (printOp v) ++ ", label %" ++ ltrue ++ ", label %" ++ lfalse   
printInstruction (LLLabel l) = "\n" ++ l ++ ":"
printInstruction (LLCall _ TVoid f p) = "call " ++ (printType TVoid) ++ " @" ++ f ++ "( " ++ (sepArgs (map (\(t,v) -> (printType t) ++ " " ++ (printOp v)) p)) ++ " )"
printInstruction (LLCall v t f p) = (printOp v) ++ " = call " ++ (printType t) ++ " @" ++ f ++ "( " ++ (sepArgs (map (\(t,v) -> (printType t) ++ " " ++ (printOp v)) p)) ++ " )"
printInstruction (LLGetElemPtr v l t id ) = (printOp v) ++ " = getelementptr [ " ++ (show l) ++ " x " ++ (printType t) ++ " ]* " ++ "@" ++ id ++ ", i32 0, i32 0"
printInstruction (LLPhi tr t ((v1,l1),(v2,l2))) = (printOp tr) ++ " = phi " ++ (printType t) ++ " [ " ++ (printOp v1) ++ ", %" ++ l1 ++ " ], [ "++ (printOp v2) ++ ", %" ++ l2 ++ " ]" 
printInstruction (Unreachable) = "unreachable"

sepArgs :: [String] -> String
sepArgs s = sepArgs' s []
  where sepArgs' [] _ = []
        sepArgs' (x:[]) ss = concat $ reverse $ x:ss
        sepArgs' (x:xs) ss = sepArgs' xs ((x++", "):ss)

printBinOp :: (Op, Type, Op, Op) -> String -> String -> String
printBinOp (tr, t, v1, v2) instr pf = (printOp tr) ++ " = " ++ instr ++ " " ++ pf ++ " " ++ (printType t) ++ " " ++ (printOp v1) ++ ", " ++ (printOp v2)
-}

printOp :: Op -> String
printOp (ODouble d)   = show d
printOp (OInteger i)  = show i
printOp (OId lid)     = "%" ++ lid

printDType :: DType -> String
printDType (DType TInt _)    = "i32"
printDType (DType TDouble _) = "double"
printDType (DType TBool _)   = "i1"
--printType t | t == TVoid      = "void"
--printType t | t == TString    = "i8"
--printType t | t == TStringP   = "i8*"

