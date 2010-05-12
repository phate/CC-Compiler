module LLVMAbs where

import ParserAbs

type LLVMId = String
type Label = String

data Op =
    OId LLVMId
  | OInteger Integer
  | ODouble Double
  deriving (Eq, Show)

data LLVMInstr =
    LLAdd Op DType Op Op
  | LLSub Op DType Op Op
  | LLMul Op DType Op Op
  | LLDiv Op DType Op Op
  | LLRem Op DType Op Op
  | LLAnd Op Op Op
  | LLOr Op Op Op
  | LLXor Op Op Op
  | LLCmp Op DType String Op Op
  | LLLoad Op DType LLVMId
  | LLStore DType Op Op
  | LLReturn DType Op
  | LLVReturn
  | LLAlloc Op DType
  | LLBr Label
  | LLLabel Label
  | LLCBr Op Label Label 
  | LLCall Op DType LLVMId [(DType,Op)]
  | LLGetElemPtr Op Int DType LLVMId
  | LLPhi Op DType ((Op,Label),(Op,Label))
  | Unreachable
  deriving (Eq, Show)
