module JVMAbs where

import ParserAbs

type LabelStr = String

type VarIndex = Integer

data JVMInstr =
   IPush Integer
 | DPush Double
 | SPush String

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
 | Iflt LabelStr
 | Ifle LabelStr
 | Ifgt LabelStr
 | Ifge LabelStr

 | Dcmpl

 | IAdd
 | ISub
 | IMul
 | IDiv
 | IRem
 | IXor

 | DAdd
 | DSub
 | DMul
 | DDiv
 
 | INeg
 | DNeg

 | ILoad Integer
 | IStore Integer
 | DLoad Integer
 | DStore Integer

 | IInc VarIndex Integer

 | Pop
 | Pop2
 
 | VReturn
 | IReturn
 | DReturn
 
 | InvokeStatic Id [Type] Type

  deriving (Eq,Show)
