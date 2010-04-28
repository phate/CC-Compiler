module LLVMAbs where

import ParserAbs

type Register = String
type Pointer = String

data Value = VInt Integer | VDouble Double | VBool Integer | VReg Register | VUndef
  deriving (Eq)
  
instance Show Value where
   show (VInt i)    = (show i)
   show (VDouble d) = (show d)
   show (VBool b)   = (show b)
   show (VReg reg)  = reg

data LLVMInstr =
   IAdd Register Value Value
 | ISub Register Value Value
 | DAdd Register Value Value
 | DSub Register Value Value
 
 | IMul Register Value Value
 | IDiv Register Value Value
 | IRem Register Value Value
 | DMul Register Value Value
 | DDiv Register Value Value
 
 | ILt Register Value Value
 | ILe Register Value Value
 | IGt Register Value Value
 | IGe Register Value Value
 | IEq Register Value Value
 | INe Register Value Value
 | BEq Register Value Value
 | BNe Register Value Value
 | DLt Register Value Value
 | DLe Register Value Value
 | DGt Register Value Value
 | DGe Register Value Value
 | DEq Register Value Value
 | DNe Register Value Value
 
 | IRet Value
 | BRet Value
 | DRet Value
 | VRet

 | IAlloca Pointer
 | IStore Value Pointer
 | ILoad Register Pointer
 | BAlloca Pointer
 | BStore Value Pointer
 | BLoad Register Pointer
 | DAlloca Pointer
 | DStore Value Pointer
 | DLoad Register Pointer
 
 | ICall Register Id [(Type, Value)]
 | BCall Register Id [(Type, Value)]
 | DCall Register Id [(Type, Value)]
 | VCall Id [(Type, Value)]

 | Br Value String String
 | UBr String
 | Unreachable

 | Label String

 | GetElemPtr Register Int Id
  deriving (Eq,Show)
  
