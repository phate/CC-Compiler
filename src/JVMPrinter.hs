module JVMPrinter where

import JVMAbs
import JVMEnv
import ParserAbs

type FileName = String

getCode :: FileName -> [JVMEnv] -> String
getCode name envs = (getHeader name) ++ (unlines $ [getFunction name e | e <- envs])


getHeader :: FileName -> String
getHeader fileName = ".class public " ++ fileName ++ "\n" ++
                     ".super java/lang/Object\n\n" ++
                     ".method public <init>()V\n" ++
                     "  aload_0\n" ++
                     "  invokespecial java/lang/Object/<init>()V\n" ++
                     "  return\n" ++
                     ".end method\n\n" ++
                     ".method public static main([Ljava/lang/String;)V\n" ++
                     ".limit locals 1\n" ++
                     "  invokestatic " ++ fileName ++ "/main()I\n" ++
                     "  pop\n" ++
                     "  return\n" ++
                     ".end method\n\n"
                     
                     
getFunction :: FileName -> JVMEnv -> String
getFunction name env = ".method public static " ++ (funName env) ++ "(" ++ (getTypes $ params env) ++ ")" ++ (getType $ returnType env) ++ "\n" ++
                       ".limit locals " ++ "50\n" ++ -- TODO!!!!!!!!!
                       ".limit stack " ++ (show $ maxStackDepth env) ++ "\n" ++
                       "  entry:\n" ++
                       unlines (getInstructions name$ instr env) ++
                       ".end method\n\n"


-- Assumes that the instructions in [JVMInstr] are in the reverse order. FileName is used for invoking
getInstructions :: FileName -> [JVMInstr] -> [String]
getInstructions name instr = 
  gI' instr [] where
    gI' :: [JVMInstr] -> [String] -> [String]
    gI' [] ss = ss
    gI' (i:is) ss = gI' is ((getInstruction name i):ss)


getInstruction :: FileName -> JVMInstr -> String
getInstruction name instr = case instr of
  IPush i         -> if (i >= -1 && i <= 5) then ("    iconst_"++(show i)) else
                      if (i >= -128 && i <= 127) then ("    bipush "++(show i)) else
                       if (i >= -32768 && i <= 32767) then ("    sipush "++(show i)) else
                        "    ldc "++(show i)
  DPush d         -> if (d == 0.0) then ("    dconst_0") else
                      if (d == 1.0) then ("    dconst_1") else
                       "    ldc2_w " ++ (show d)
                       
  SPush s         -> "    ldc " ++ s
  
  Label label     -> "  " ++ label ++ ":"

  Goto label      -> "    goto " ++ label

  If_icmplt label -> "    if_icmplt " ++ label
  If_icmple label -> "    if_icmple " ++ label
  If_icmpgt label -> "    if_icmpgt " ++ label
  If_icmpge label -> "    if_icmpge " ++ label
  If_icmpeq label -> "    if_icmpeq " ++ label
  If_icmpne label -> "    if_icmpne " ++ label

  Ifne label      -> "    ifne " ++ label
  Ifeq label      -> "    ifeq " ++ label
  Iflt label      -> "    iflt " ++ label
  Ifle label      -> "    ifle " ++ label
  Ifgt label      -> "    ifgt " ++ label
  Ifge label      -> "    ifge " ++ label

  Dcmpl           -> "    dcmpl"

  IAdd            -> "    iadd"
  ISub            -> "    isub"
  IMul            -> "    imul"
  IDiv            -> "    idiv"
  IRem            -> "    irem"
  IXor            -> "    ixor"

  DAdd            -> "    dadd"
  DSub            -> "    dsub"
  DMul            -> "    dmul"
  DDiv            -> "    ddiv"
  
  INeg            -> "    ineg"
  DNeg            -> "    dneg"

  ILoad index     -> if (index <= 3) then ("    iload_"++(show index)) else
                      "    iload " ++ (show index)
  IStore index    -> if (index <= 3) then ("    istore_"++(show index)) else
                      "    istore " ++ (show index)
  DLoad index     -> if (index <= 3) then ("    dload_"++(show index)) else
                      "    dload " ++ (show index)
  DStore index    -> if (index <= 3) then ("    dstore_"++(show index)) else
                      "    dstore " ++ (show index)

  IInc index val  -> "    iinc " ++ show(index) ++ " " ++ show(val)

  Pop             -> "    pop"
  Pop2            -> "    pop2"
  
  VReturn         -> "    return"
  IReturn         -> "    ireturn"
  DReturn         -> "    dreturn"
  
  InvokeStatic id typs ret ->
    ("    invokestatic " ++ name' ++  "/" ++ id ++ "(" ++ getTypes typs ++ ")" ++ (getType ret))
      where
        name' = if (id == "printInt" || id == "printDouble" || id == "printString" || id == "readInt" || id == "readDouble")
                   then "Runtime" else name


getTypes :: [Type] -> String
getTypes []     = ""
getTypes (t:ts) = (getType t) ++ (getTypes ts)

getType :: Type -> String
getType t = case t of
  TInt    -> "I"
  TBool   -> "I"
  TDouble -> "D"
  TVoid   -> "V"
  TString -> "Ljava/lang/String;"
