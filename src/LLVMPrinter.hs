module LLVMPrinter where

import LLVMAbs
import LLVMEnv
import ParserAbs(DType(..),Type(..))
import Data.Map(toList)

getCode :: LLVMEnv -> String
getCode env = printHeader env ++ (printStructs $ structs env) ++ (unlines $ [printFunction e | e <- fctEnv env ])

printHeader :: LLVMEnv -> String
printHeader env = "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32\"\n\n" ++ printPreDefFun ++ 
                  (printGlobStrings $ globStrings env)

printPreDefFun :: String
printPreDefFun =  "\n\ndeclare void @printInt(i32)\n" ++
                  "declare void @printDouble(double)\n" ++ 
                  "declare void @printString(i8*)\n" ++
                  "declare i32 @readInt()\n" ++
                  "declare double @readDouble()\n" ++
                  "declare i8* @calloc(i32, i32)\n\n"

printGlobStrings ::[(LLVMId, String)] -> String
printGlobStrings = concatMap  (\(id, s) -> "@" ++ id ++ " = internal constant [ " ++
                              (show $ (length s)+1) ++ " x i8] c\"" ++ s ++ "\\00\"\n")

printStructs :: StructMap -> String
printStructs psMap = pS' (Data.Map.toList psMap)
  where
    pS' [] = "\n"
    pS' ((id, fields):pp) = "%struct." ++ id ++ " = type { " ++ (sepArgs $ (fieldTypes fields)) ++ " }\n" ++ (pS' pp)
    fieldTypes [] = []
    fieldTypes ((id, t):ff) = (printDType t):(fieldTypes ff)

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
printInstructions instr = map printInstruction (reverse instr)

printInstruction :: LLVMInstr -> String
printInstruction (LLAdd tr (DType TInt 0) v1 v2) = printBinOp (tr,(DType TInt 0),v1,v2) "add" ""
printInstruction (LLAdd tr (DType TDouble 0) v1 v2) = printBinOp (tr,(DType TDouble 0),v1,v2) "fadd" ""
printInstruction (LLSub tr (DType TInt 0) v1 v2) = printBinOp (tr,(DType TInt 0),v1,v2) "sub" ""
printInstruction (LLSub tr (DType TDouble 0) v1 v2) = printBinOp (tr,(DType TDouble 0),v1,v2) "fsub" ""
printInstruction (LLMul tr (DType TInt 0) v1 v2) = printBinOp (tr,(DType TInt 0),v1,v2) "mul" ""
printInstruction (LLMul tr (DType TDouble 0) v1 v2) = printBinOp (tr,(DType TDouble 0),v1,v2) "fmul" ""
printInstruction (LLDiv tr (DType TInt 0) v1 v2) = printBinOp (tr,(DType TInt 0),v1,v2) "sdiv" ""
printInstruction (LLDiv tr (DType TDouble 0) v1 v2) = printBinOp (tr,(DType TDouble 0),v1,v2) "fdiv" ""
printInstruction (LLRem tr (DType TInt 0) v1 v2) = printBinOp (tr,(DType TInt 0),v1,v2) "srem" ""
printInstruction (LLRem tr (DType TDouble 0) v1 v2) = printBinOp (tr,(DType TDouble 0),v1,v2) "frem" ""
printInstruction (LLAnd tr v1 v2) = printBinOp (tr,(DType TBool 0),v1,v2) "and" ""
printInstruction (LLOr tr v1 v2) = printBinOp (tr,(DType TBool 0),v1,v2) "or" ""
printInstruction (LLXor tr v1 v2) = printBinOp (tr,(DType TBool 0),v1,v2) "xor" ""
printInstruction (LLCmp tr (DType TInt 0) pf v1 v2) = printBinOp (tr,(DType TInt 0),v1,v2) "icmp" pf
printInstruction (LLCmp tr (DType TBool 0) pf v1 v2) = printBinOp (tr,(DType TBool 0),v1,v2) "icmp" pf
printInstruction (LLCmp tr (DType TDouble 0) pf v1 v2) = printBinOp (tr,(DType TDouble 0),v1,v2) "fcmp" pf
printInstruction (LLCmp tr (TIdent id) pf v1 v2) = printBinOp (tr, (TIdent id), v1, v2) "icmp" pf
printInstruction (LLLoad tr t lid) = (printOp tr) ++ " = " ++ "load " ++ (printDType t) ++ "*" ++ " %" ++ lid
printInstruction (LLStore t v1 v2) = "store " ++ (printDType t) ++ " " ++ (printOp v1) ++ ", " ++ (printDType t) ++ "* " ++ (printOp v2)
printInstruction (LLReturn t v) = "ret " ++ (printDType t) ++ " " ++ (printOp v)
printInstruction LLVReturn = "ret void"
printInstruction (LLAlloc tr t) = (printOp tr) ++ " = " ++ "alloca " ++ (printDType t)
printInstruction (LLBr l) = "br label " ++ "%" ++ l
printInstruction (LLCBr v ltrue lfalse) = "br i1 " ++ (printOp v) ++ ", label %" ++ ltrue ++ ", label %" ++ lfalse   
printInstruction (LLLabel l) = "\n" ++ l ++ ":"
printInstruction (LLCall _ TVoid f p) = "call " ++ (printDType TVoid) ++ " @" ++ f ++ "( " ++ (sepArgs (map (\(t,v) -> (printDType t) ++ " " ++ (printOp v)) p)) ++ " )"
printInstruction (LLCall v t f p) = (printOp v) ++ " = call " ++ (printDType t) ++ " @" ++ f ++ "( " ++ (sepArgs (map (\(t,v) -> (printDType t) ++ " " ++ (printOp v)) p)) ++ " )"
printInstruction (LLGetElemPtrString v l t id ) = (printOp v) ++ " = getelementptr [ " ++ (show l) ++ " x " ++ (printDType t) ++ " ]* " ++ "@" ++ id ++ ", i32 0, i32 0"
printInstruction (LLPhi tr t ((v1,l1),(v2,l2))) = (printOp tr) ++ " = phi " ++ (printDType t) ++ " [ " ++ (printOp v1) ++ ", %" ++ l1 ++ " ], [ "++ (printOp v2) ++ ", %" ++ l2 ++ " ]" 
printInstruction (Unreachable) = "unreachable"
printInstruction (LLGetElemPtr d t1 v1 t2 v2) = (printOp d) ++ " = getelementptr " ++ (printDType t1) ++ " " ++ (printOp v1) ++ ", i32 0, " ++ (printDType t2) ++ " " ++ (printOp v2)
printInstruction (LLBitcast d t1 v t2) = (printOp d) ++ " = bitcast " ++ (printDType t1) ++ " " ++ (printOp v) ++ " to " ++ (printDType t2)


printBinOp :: (Op, DType, Op, Op) -> String -> String -> String
printBinOp (tr, t, v1, v2) instr pf = (printOp tr) ++ " = " ++ instr ++ " " ++ pf ++ " " ++ (printDType t) ++ " " ++ (printOp v1) ++ ", " ++ (printOp v2)


printOp :: Op -> String
printOp (ODouble d)   = show d
printOp (OInteger i)  = show i
printOp (OId lid)     = "%" ++ lid
printOp (ONull)   = "null"

printDType :: DType -> String
printDType (DType TInt 0)    = "i32"
printDType (DType TDouble 0) = "double"
printDType (DType TBool 0)   = "i1"
printDType (TIdent id) = "%struct." ++ id ++ "*"
printDType (TVoid) = "void"
printDType (TPtr8) = "i8*"
printDType (TString) = "i8"

printDType (TArr t i) = "[ 0 x " ++ printDType (DType t i) ++ "]*"
printDType (DType t i) = "{i32, [ 0 x " ++ (printDType (DType t (i-1))) ++ "] }*"


