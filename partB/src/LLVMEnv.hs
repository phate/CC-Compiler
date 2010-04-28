module LLVMEnv where

import Control.Monad
import Control.Monad.State
import Data.Map
import ParserAbs
import LLVMAbs

{----- Types/data -----}
type Index = Integer
type Context = Map Id Pointer
type Env = [Context]

data LLVMEnv = LLVMEnv 
    {
     globalConstants :: [(Id, String)],
     globalCounter :: Integer,
     functionName :: Id,
     parameters :: [(Type, Id)],
     returnType :: Type,
     labelCounter :: Integer,
     regCounter :: Integer,
     varCounter :: Integer,
     environment :: Env,
     instructions :: [LLVMInstr]
    } 
    deriving Show

type S a = State LLVMEnv a

{----- END Types/data -----}


{----- Environment functions -----}
newEnvironment :: LLVMEnv
newEnvironment = LLVMEnv { functionName = "", parameters = [], returnType = TVoid, labelCounter = 1, regCounter = 0,
varCounter = 0, environment = [empty], instructions = [], globalConstants = [], globalCounter = 0 }

add :: LLVMInstr -> S ()
add i = do state <- get
           let prevInstr = instructions state
           put $ state { instructions = i:prevInstr }

addGlobal :: String -> S Id
addGlobal str = do state <- get
                   let consts = globalConstants state
                   let count = globalCounter state
                   let fName = functionName state
                   let gName = "@_" ++ fName ++ "_global" ++ (show count)
                   put $ state { globalConstants = ((gName, str):consts), globalCounter = count + 1 }
                   return gName

addVar :: Id -> S Pointer
addVar x = do state <- get
              let (scope:rest) = environment state
              let varNum = varCounter state
              let var = "%var" ++ (show varNum)
              put $ state { environment = ((insert x var scope):rest), varCounter = varNum + 1 }
              return var

 
lookupVar :: Id -> S Pointer
lookupVar x = do state <- get
                 let (scope:rest) = environment state
                 lookupVar' (scope:rest) where
                   lookupVar' :: [Context] -> S Pointer
                   lookupVar' (c:cs) = case Data.Map.lookup x c of
                                         Nothing     -> lookupVar' cs
                                         Just ptr    -> return ptr

getLabel :: S String
getLabel = do state <- get
              let count = labelCounter state
              let label = "lab" ++ (show count)
              put $ state { labelCounter = count + 1 }
              return label
              
getRegister :: S Register
getRegister = do state <- get
                 let count = regCounter state
                 let register = "%t" ++ (show count)
                 put $ state {regCounter = count + 1 }
                 return register

pushScope :: S ()
pushScope = do state <- get
               let cont = environment state
               put $ state { environment = (empty:cont) }

popScope :: S ()
popScope = do state <- get
              let (scope:rest) = environment state
              put $ state { environment = (rest) }
                
getFunctionReturnType :: S Type
getFunctionReturnType = do state <- get
                           return (returnType state)
                   
setFunctionReturnType :: Type -> S ()
setFunctionReturnType t = do state <- get
                             put $ state { returnType = t }
                     
setFunctionName :: Id -> S ()
setFunctionName name = do state <- get
                          put $ state { functionName = name }

--setFunctionParameters :: [(Type, Id)] -> S ()
--setFunctionParameters params = do state <- get
--                                  put $ state { parameters = params }


putParameter :: Type -> Id -> Id -> [LLVMInstr] -> S ()
putParameter t var pName is = 
  do state <- get
     let params = parameters state
     let instr = instructions state
     put $ state { parameters = ((t, pName):params), instructions = (is ++ instr) }

setFunctionParameters :: [(Type, Id)] -> S ()
setFunctionParameters params = setFunPar 0 params where
  setFunPar :: Integer -> [(Type, Id)] -> S ()
  setFunPar count []           = return ()
  setFunPar count ((t, id):xs) = 
    do varName <- addVar id
       let pName = "%p" ++ (show count)
       let instr = case t of
                     TInt    -> [(IStore (VReg pName) varName), (IAlloca varName)]
                     TBool   -> [(BStore (VReg pName) varName), (BAlloca varName)]
                     TDouble -> [(DStore (VReg pName) varName), (DAlloca varName)]
       --let i2 = (IAlloca varName)
       --let instr = (IStore (VReg pName) varName):[i2]
       putParameter t varName pName instr
       setFunPar (count + 1) xs


fixLabels :: S ()
fixLabels = do state <- get
               let (i:instr) = instructions state
               case i of
                 Label l -> add Unreachable
                 _       -> return ()
               

{----- END Environment functions -----}
