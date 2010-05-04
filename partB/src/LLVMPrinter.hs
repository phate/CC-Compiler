module LLVMPrinter where

import LLVMAbs
import LLVMEnv
import ParserAbs

type FileName = String

getCode :: [LLVMEnv] -> String
getCode envs = getGlobals envs ++ getHeader  ++ (unlines $ [getFunction e | e <- envs])

getGlobals :: [LLVMEnv] -> String
getGlobals []       = ""
getGlobals (e:envs) = getGlobals' (globalConstants e) ++ getGlobals envs where
  getGlobals' :: [(Id, String)] -> String
  getGlobals' []             = ""
  getGlobals' ((id, str):gs) = 
    id ++ " = internal constant [ " ++ (show (length str)) 
      ++ " x i8 ] c" ++ filter (\c -> c /= '"') str) ++ "\n" ++ getGlobals' gs


getHeader :: String
getHeader = "declare void @printInt(i32)\n" ++
            "declare void @printDouble(double)\n" ++
            "declare void @printString(i8*)\n" ++
            "declare i32 @readInt()\n" ++
            "declare double @readDouble()\n\n"
                     
                     
getFunction :: LLVMEnv -> String
getFunction env = "define " ++ (tempGetRetType $ returnType env) ++ " @" ++ (functionName env) ++ "(" ++ (getTypes $ parameters env) ++ ") {\n" ++
                  "entry:" ++
                  unlines (getInstructions $ instructions env) ++
                  "}\n\n"


-- Assumes that the instructions in [LLVMInstr] are in the reverse order
getInstructions :: [LLVMInstr] -> [String]
getInstructions instr = 
  gI' instr [] where
    gI' :: [LLVMInstr] -> [String] -> [String]
    gI' [] ss = ss
    gI' (i:is) ss = gI' is ((getInstruction i):ss)


getInstruction :: LLVMInstr -> String
getInstruction instr = case instr of

  IAdd d v1 v2     -> "\t" ++ d ++ " = add i32 " ++ (show v1) ++ ", " ++ (show v2)
  ISub d v1 v2     -> "\t" ++ d ++ " = sub i32 " ++ (show v1) ++ ", " ++ (show v2)
  
  DAdd d v1 v2     -> "\t" ++ d ++ " = add double " ++ (show v1) ++ ", " ++ (show v2)
  DSub d v1 v2     -> "\t" ++ d ++ " = sub double " ++ (show v1) ++ ", " ++ (show v2)
  
  
  IMul d v1 v2     -> "\t" ++ d ++ " = mul i32 " ++ (show v1) ++ ", " ++ (show v2)
  IDiv d v1 v2     -> "\t" ++ d ++ " = sdiv i32 " ++ (show v1) ++ ", " ++ (show v2)
  IRem d v1 v2     -> "\t" ++ d ++ " = srem i32 " ++ (show v1) ++ ", " ++ (show v2)
  
  DMul d v1 v2     -> "\t" ++ d ++ " = mul double " ++ (show v1) ++ ", " ++ (show v2)
  DDiv d v1 v2     -> "\t" ++ d ++ " = fdiv double " ++ (show v1) ++ ", " ++ (show v2)
  
  
  IRet v           -> "\tret i32 " ++ (show v)
  BRet v           -> "\tret i1 " ++ (show v)
  DRet v           -> "\tret double " ++ (show v)
  VRet             -> "\tret void"

  IAlloca ptr      -> "\t" ++ ptr ++ " = alloca i32"
  IStore v ptr     -> "\tstore i32 " ++  (show v) ++ ", i32* " ++ ptr
  ILoad d ptr      -> "\t" ++ d ++ " = load i32* " ++ ptr
  BAlloca ptr      -> "\t" ++ ptr ++ " = alloca i1"
  BStore v ptr     -> "\tstore i1 " ++  (show v) ++ ", i1* " ++ ptr
  BLoad d ptr      -> "\t" ++ d ++ " = load i1* " ++ ptr
  DAlloca ptr      -> "\t" ++ ptr ++ " = alloca double"
  DStore v ptr     -> "\tstore double " ++  (show v) ++ ", double* " ++ ptr
  DLoad d ptr      -> "\t" ++ d ++ " = load double* " ++ ptr
  
  
  ICall d fun args -> "\t" ++ d ++ " = call i32 @" ++ fun ++ "(" ++ getArgsTemp args ++ ")"
  BCall d fun args -> "\t" ++ d ++ " = call i1 @" ++ fun ++ "(" ++ getArgsTemp args ++ ")"
  DCall d fun args -> "\t" ++ d ++ " = call double @" ++ fun ++ "(" ++ getArgsTemp args ++ ")"
  VCall fun args   -> "\tcall void @" ++ fun ++ "(" ++ getArgsTemp args ++ ")"
  
  
  ILt d v1 v2      -> "\t" ++ d ++ " = icmp slt i32 " ++ (show v1) ++ ", " ++ (show v2)
  ILe d v1 v2      -> "\t" ++ d ++ " = icmp sle i32 " ++ (show v1) ++ ", " ++ (show v2)
  IGt d v1 v2      -> "\t" ++ d ++ " = icmp sgt i32 " ++ (show v1) ++ ", " ++ (show v2)
  IGe d v1 v2      -> "\t" ++ d ++ " = icmp sge i32 " ++ (show v1) ++ ", " ++ (show v2)
  IEq d v1 v2      -> "\t" ++ d ++ " = icmp eq i32 " ++ (show v1) ++ ", " ++ (show v2)
  INe d v1 v2      -> "\t" ++ d ++ " = icmp ne i32 " ++ (show v1) ++ ", " ++ (show v2)
  
  BEq d v1 v2      -> "\t" ++ d ++ " = icmp eq i1 " ++ (show v1) ++ ", " ++ (show v2)
  BNe d v1 v2      -> "\t" ++ d ++ " = icmp ne i1 " ++ (show v1) ++ ", " ++ (show v2)
  
  DLt d v1 v2      -> "\t" ++ d ++ " = fcmp olt double " ++ (show v1) ++ ", " ++ (show v2)
  DLe d v1 v2      -> "\t" ++ d ++ " = fcmp ole double " ++ (show v1) ++ ", " ++ (show v2)
  DGt d v1 v2      -> "\t" ++ d ++ " = fcmp ogt double " ++ (show v1) ++ ", " ++ (show v2)
  DGe d v1 v2      -> "\t" ++ d ++ " = fcmp oge double " ++ (show v1) ++ ", " ++ (show v2)
  DEq d v1 v2      -> "\t" ++ d ++ " = fcmp oeq double " ++ (show v1) ++ ", " ++ (show v2)
  DNe d v1 v2      -> "\t" ++ d ++ " = fcmp one double " ++ (show v1) ++ ", " ++ (show v2)

  Br v tLab fLab   -> "\tbr i1 " ++ (show v) ++ ", label %" ++ tLab ++ ", label %" ++ fLab
  UBr lab          -> "\tbr label %" ++ lab
  Unreachable      -> "\tunreachable"

  Label lab        -> lab ++ ":"

  GetElemPtr d len g -> "\t" ++ d ++ " = getelementptr [ " ++ (show len) ++ " x i8 ]* " ++ g ++ ", i32 0, i32 0"


tempGetRetType :: Type -> String
tempGetRetType t = case t of
  TInt    -> "i32"
  TBool   -> "i1"
  TDouble -> "double"
  TVoid   -> "void"
  TString -> "i8*"


getArgsTemp :: [(Type, Value)] -> String
getArgsTemp [] = ""
getArgsTemp ((t, v):xs) = tempGetRetType t ++ " " ++ (show v) ++ (getArgsTemp xs)

getTypes :: [(Type, Id)] -> String
getTypes []     = ""
--getTypes (t:ts) = (getType t) ++ ", " ++ (getTypes ts)
getTypes (t:ts) = (getType t)


getType :: (Type, Id) -> String
getType (t, id) = case t of
  TInt    -> "i32 " ++ id
  TBool   -> "i1 " ++ id
  TDouble -> "double " ++ id
  TVoid   -> "void " ++ id
