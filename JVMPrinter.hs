module JVMPrinter where

import JVMAbs


-- Assumes that the instructions in [JVMInstr] are in the reverse order
generateCode :: [JVMInstr] -> [String]
generateCode instr = 
  gC' instr [] where
    gC' :: [JVMInstr] -> [String] -> [String]
    gC' [] ss = ss
    gC' (i:is) ss = gC' is ((genSingle i):ss)


genSingle :: JVMInstr -> String
genSingle instr = case instr of
  IPush i         -> if (i >= -1 && i <= 5) then ("  iconst_"++(show i)) else
                      if (i >= -128 && i <= 127) then ("  bipush "++(show i)) else
                       if (i >= -32768 && i <= 32767) then ("  sipush "++(show i)) else
                        "  ldc "++(show i)
  DPush d         -> if (d == 0.0) then ("  dconst_0") else
                      if (d == 1.0) then ("  dconst_1") else
                       "  ldc2_w " ++ (show d)
  
  Label label     -> label ++ ":"

  Goto label      -> "  goto " ++ label

  If_icmplt label -> "  if_icmplt " ++ label
  If_icmple label -> "  if_icmple " ++ label
  If_icmpgt label -> "  if_icmpgt " ++ label
  If_icmpge label -> "  if_icmpge " ++ label
  If_icmpeq label -> "  if_icmpeq " ++ label
  If_icmpne label -> "  if_icmpne " ++ label

  Ifne label      -> "  ifne " ++ label
  Ifeq label      -> "  ifeq " ++ label

  Dcmpl           -> "  dcmpl"
  Dcmpg           -> "  dcmpg"

  IAdd            -> "  iadd"
  ISub            -> "  isub"
  IMul            -> "  imul"
  IDiv            -> "  idiv"
  IRem            -> "  irem"
  IAnd            -> "  iand"
  IOr             -> "  ior"

  DAdd            -> "  dadd"
  DSub            -> "  dsub"
  DMul            -> "  dmul"
  DDiv            -> "  ddiv"

  ILoad index     -> if (index <= 3) then ("  iload_"++(show index)) else
                      "  iload " ++ (show index)
  IStore index    -> if (index <= 3) then ("  istore_"++(show index)) else
                      "  istore " ++ (show index)
  DLoad index     -> if (index <= 3) then ("  dload_"++(show index)) else
                      "  dload " ++ (show index)
  DStore index    -> if (index <= 3) then ("  dstore_"++(show index)) else
                      "  dstore " ++ (show index)

  IInc index val  -> "  iinc " ++ show(index) ++ " " ++ show(val)

  Pop             -> "  pop"

