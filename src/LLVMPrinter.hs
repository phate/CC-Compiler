module LLVMPrinter where

import LLVMAbs
import LLVMEnv
import ParserAbs(Type(..))

getCode :: [LLVMEnv] -> String
getCode envs = (printHeader envs) ++ (unlines $ [printFunction e | e <- envs ])

printHeader :: [LLVMEnv] -> String
printHeader envs =  (concatMap
                      (\(id,s) -> "@" ++ id ++ " = internal constant [ " ++
                      (show $ ((length s)+1)) ++ " x i8] c\"" ++ s ++ "\\00\"\n")
                      (concat [ globConst e | e <- envs ])) ++
                    "\n\ndeclare void @printInt(i32)\n" ++
                    "declare void @printDouble(double)\n" ++ 
                    "declare void @printString(i8*)\n" ++
                    "declare i32 @readInt()\n" ++
                    "declare double @readDouble()\n\n" 

printFunction :: LLVMEnv -> String
printFunction env = "define " ++ (printType $ returnType env) ++
                    " @" ++ funName env ++
                    "( " ++ (sepArgs args) ++
                    " ){" ++
                    (unlines $ printInstructions $ instr env) ++ "}\n"
  where args = zipWith (\(id,t) i -> (printType t) ++ " " ++ (printOp (OL $ createPLLVMId $ id2LLVMId id i)) ) (params env) [0..]


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

printOp :: Op -> String
printOp (OD d)    = show d
printOp (OI i)    = show i
printOp (OL lid)  = "%" ++ lid

printType :: Type -> String
printType t | t == TInt 0     = "i32"
printType t | t == TDouble 0  = "double"
printType t | t == TBool 0    = "i1"
printType t | t == TVoid      = "void"
printType t | t == TString    = "i8"
printType t | t == TStringP   = "i8*"
