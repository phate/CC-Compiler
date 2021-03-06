module LLVMEnv where

import LLVMAbs
import ParserAbs

import Data.Map
import Data.List
import Control.Monad
import Control.Monad.State

type Env = Map Id (LLVMId,DType)
type StructMap = Map Id [(Id, DType)]


data FctEnv = FctEnv
  {
    funName :: Id,
    params :: [(DType,Id)],
    returnType :: DType,
    instr :: [LLVMInstr],
    env :: [Env],
    labelCounter :: Integer,
    llvmIdCounter :: Integer
  }
  deriving Show

newFctEnv :: FctEnv
newFctEnv = FctEnv { funName = "", returnType = TVoid, params = [], instr = [LLLabel "lab0"], env = [empty], labelCounter = 1, llvmIdCounter = 0 }


data LLVMEnv = LLVMEnv
  {
    fctEnv :: [FctEnv],
    globStrings :: [(LLVMId, String)],
    globArrayTypes :: Map DType LLVMId,
    structs :: StructMap
  }
  deriving Show

newLLVMEnv :: LLVMEnv
newLLVMEnv = LLVMEnv { fctEnv = [], globStrings = [], globArrayTypes = empty, structs = empty }

type S a = State LLVMEnv a

newFct :: DType -> Id -> [(DType,Id)] -> S()
newFct t id ps = do s <- get
                    let fct = newFctEnv { funName = id, returnType = t, params = ps }
                    put $ s { fctEnv = fct:(fctEnv s) }  

getLastInstr :: S LLVMInstr
getLastInstr = do s <- get
                  let fct:fcts = fctEnv s
                  let i:is = instr fct
                  return i

addParams :: [(DType,Id)] -> S ()
addParams [] = return ()
addParams ((t,id):ps) = do  addVar id t
                            addParams ps 
                
addVar :: Id -> DType -> S LLVMId
addVar v t = do s <- get
                let fct:fcts = fctEnv s
                let e:es = env fct
                let lic = llvmIdCounter fct
                let llvmid = id2LLVMId v lic
                let newEnv = Data.Map.insert v (llvmid,t) e
                put $ s { fctEnv = (fct { env = newEnv:es, llvmIdCounter = lic+1 }):fcts }
                return llvmid

lookupVar :: Id -> S (LLVMId,DType)
lookupVar v = do  s <- get
                  let fct:fcts = fctEnv s
                  let ev = env fct
                  return (lookupVar' v ev)
  where
    lookupVar' :: Id -> [Env] -> (LLVMId,DType)
    lookupVar' v [] = error $ "this should not happen:" ++ v
    lookupVar' v (e:env) = case Data.Map.lookup v e of
                            Nothing -> lookupVar' v env
                            Just r  -> r 
createLLVMId :: S LLVMId
createLLVMId = do s <- get
                  let fct:fcts = fctEnv s
                  let lic = llvmIdCounter fct
                  let lid = show lic 
                  put $ s { fctEnv = (fct { llvmIdCounter = lic + 1 }):fcts }
                  return ("t" ++ lid)

addInstr :: LLVMInstr -> S()
addInstr i = do s <- get
                let fct:fcts = fctEnv s 
                put $ s { fctEnv = (fct { instr = i:(instr fct) }):fcts }

pushScope :: S()
pushScope = do  s <- get
                let fct:fcts = fctEnv s
                let sc = env fct
                put $ s { fctEnv = (fct { env = (empty:sc) }):fcts }

popScope :: S()
popScope = do s <- get
              let fct:fcts = fctEnv s
              let sc:ssc = env fct
              put $ s { fctEnv = (fct { env = ssc }):fcts }

createLabel :: S Label
createLabel = do  s <- get
                  let fct:fcts = fctEnv s
                  let lc = labelCounter fct
                  let l = "lab" ++ (show lc)
                  put $ s { fctEnv = (fct { labelCounter = lc + 1 }):fcts }
                  return l

addArrayType :: DType -> S LLVMId
addArrayType = undefined

addGlobString :: String -> S LLVMId
addGlobString str = do  s <- get
                        let fct:fcts = fctEnv s
                        let fname = funName fct
                        let lid = "_" ++ (funName fct) ++ "string" ++ (show $ length $ globStrings s)
                        put $ s { globStrings = (lid,str):(globStrings s) }
                        return lid 

getLastLabel :: S Label
getLastLabel = do s <- get
                  let fct:fcts = fctEnv s
                  let inst = instr fct
                  return $ [ l | i@(LLLabel l) <- inst ]!!0

createPLLVMId :: LLVMId -> LLVMId
createPLLVMId id = "_p_" ++ id

id2LLVMId :: Id -> Integer -> LLVMId
id2LLVMId x i = "var" ++ x ++ show(i)

-- Get the offset for field in the struct type def'd by ptr
getFieldOffset :: Id -> Id -> S Integer
getFieldOffset field struct = 
  do s <- get
     let strs = structs s
     case Data.Map.lookup struct strs of
       Nothing -> error $ "getFieldOffset: Something went really wrong here.."
       Just fields -> case elemIndex field [ f | (f, t) <- fields]  of
                        Nothing -> error $ "getFieldOffset: Something went really wrong here.."  
                        Just i  -> return (toInteger i)

-- Get the size of a struct
getStructSize :: Id -> S Integer
getStructSize struct =
  do s <- get
     let strs = structs s
     case Data.Map.lookup struct strs of
       Nothing -> error $ "getStructSize: Something went really wrong here.."
       Just fields -> return (countSize [ t | (f, t) <- fields ] 0)
  where
    countSize [] n = n
    countSize (t:tt) n = case t of
                         (DType TDouble 0) -> countSize tt (n+8)
                         (DType TBool 0)   -> countSize tt (n+1)
                         _                 -> countSize tt (n+4)

-- Adds a struct to the struct map
addStruct :: Id -> [(Id, DType)] -> S ()
addStruct id decls = do s <- get
                        let strs = structs s
                        put $ s { structs = Data.Map.insert id decls strs }


getReturnType :: S DType
getReturnType = do s <- get
                   let (currFct:fcts) = fctEnv s
                   let ret = returnType currFct
                   return ret
                   
