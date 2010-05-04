module LLVMAbs where

import ParserAbs

type LLVMId = String
type Label = String

data Op =
    OL LLVMId
  | OI Integer
  | OD Double
  deriving (Eq, Show)

data LLVMInstr =
    LLAdd Op Type Op Op
  | LLSub Op Type Op Op
  | LLMul Op Type Op Op
  | LLDiv Op Type Op Op
  | LLRem Op Type Op Op
  | LLAnd Op Op Op
  | LLOr Op Op Op
  | LLXor Op Op Op
  | LLCmp Op Type String Op Op
  | LLLoad Op Type LLVMId
  | LLStore Type Op Op
  | LLReturn Type Op
  | LLVReturn
  | LLAlloc Op Type
  | LLBr Label
  | LLLabel Label
  | LLCBr Op Label Label 
  | LLCall Op Type LLVMId [(Type,Op)]
  | LLGetElemPtr Op Int Type LLVMId
  | LLPhi Op Type ((Op,Label),(Op,Label))
  | Unreachable
  deriving (Eq, Show)
