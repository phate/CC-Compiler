module LLVMEnv where

import LLVMAbs
import ParserAbs

import Data.Map
import Control.Monad
import Control.Monad.State

type Env = Map Id (LLVMId,Type)

data LLVMEnv = LLVMEnv
  {
    funName :: Id,
    params :: [(Id,Type)],
    returnType :: Type,
    instr :: [LLVMInstr],
    env :: [Env],
    labelCounter :: Integer, 
    llvmIdCounter :: Integer,
    globConst :: [(LLVMId,String)]
  }
  deriving Show

type S a = State LLVMEnv a

newEnv :: LLVMEnv
newEnv = LLVMEnv { funName = "", returnType = TVoid, params = [], instr = [LLLabel "lab0"], globConst = [], env = [empty], llvmIdCounter = 0, labelCounter = 1 }

addGlobConst :: LLVMId -> String -> S()
addGlobConst i c = do s <- get
                      put $ s { globConst = (i,c):(globConst s) }

addInstr :: LLVMInstr -> S()
addInstr i = do s <- get
                put $ s { instr = i:(instr s) }

addVar :: Id -> Type -> S LLVMId
addVar v t = do s <- get
                let (e:es) = env s
                let lic = llvmIdCounter s
                let llvmid = id2LLVMId v lic
                let newEnv = Data.Map.insert v (llvmid,t) e
                put $ s { env = (newEnv:es), llvmIdCounter = lic+1 }
                return llvmid

lookupVar :: Id -> S (LLVMId,Type)
lookupVar v = do  s <- get
                  let ev = env s
                  return (lookupVar' v ev)
  where
    lookupVar' :: Id -> [Env] -> (LLVMId,Type)
    lookupVar' v [] = error $ "this should not happen:" ++ v
    lookupVar' v (e:env) = case Data.Map.lookup v e of
                            Nothing -> lookupVar' v env
                            Just r  -> r 

id2LLVMId :: Id -> Integer -> LLVMId
id2LLVMId x i = "var" ++ x ++ show(i)

createLLVMId :: S LLVMId
createLLVMId = do s <- get
                  let lic = llvmIdCounter s
                  let lid = show lic 
                  put $ s { llvmIdCounter = lic + 1 }
                  return ("t" ++ lid)

createPLLVMId :: LLVMId -> LLVMId
createPLLVMId i = "_p_" ++ i

createLabel :: S Label
createLabel = do  s <- get
                  let lc = labelCounter s
                  let l = "lab" ++ (show lc)
                  put $ s { labelCounter = lc + 1 }
                  return l

getLastLabel :: S Label
getLastLabel = do s <- get
                  let inst = instr s
                  return $ [ l | i@(LLLabel l) <- inst ]!!0

pushScope :: S()
pushScope = do  s <- get
                let sc = env s
                put $ s { env = (empty:sc) }

popScope :: S()
popScope = do s <- get
              let sc:ssc = env s
              put $ s { env = ssc }

setFunctionName :: Id -> S()
setFunctionName n = do  s <- get
                        put $ s { funName = n }

setParameters :: [(Id,Type)] -> S()
setParameters ps = do s <- get
                      put $ s { params = ps }
                      return ()

getReturnType :: S Type
getReturnType = do  s <- get
                    return (returnType s)
  
setReturnType :: Type -> S()
setReturnType t = do  s <- get
                      put $ s { returnType = t }                
