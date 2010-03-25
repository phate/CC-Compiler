module JVMAbs where

type LabelStr = String

type VarIndex = Integer

data JVMInstr =
   IPush Integer
 | DPush Double

 | Label LabelStr

 | Goto LabelStr

 | If_icmplt LabelStr
 | If_icmple LabelStr
 | If_icmpgt LabelStr
 | If_icmpge LabelStr
 | If_icmpeq LabelStr
 | If_icmpne LabelStr

 | Ifne LabelStr
 | Ifeq LabelStr

 | Dcmpl
 | Dcmpg

 | IAdd
 | ISub
 | IMul
 | IDiv
 | IRem
 | IAnd
 | IOr

 | DAdd
 | DSub
 | DMul
 | DDiv

 | ILoad Integer
 | IStore Integer
 | DLoad Integer
 | DStore Integer

 | IInc VarIndex Integer

 | Pop

  deriving (Eq,Ord,Show)
