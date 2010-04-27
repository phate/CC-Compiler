module LLVMEnv where

import Control.Monad
import Control.Monad.State
import Data.Map
import ParserAbs
import LLVMAbs

{----- Types/data -----}
type Index = Integer
type Context = Map Id (Index,Type)
type Env = [Context]

data LLVMEnv = LLVMEnv 
    {
     functionName :: Id,
     parameters :: [(Type, Id)],
     returnType :: Type,
     labelCounter :: Integer,
     regCounter :: Integer,
     environment :: Env,
     instructions :: [LLVMInstr]
    } 
    deriving Show

type S a = State LLVMEnv a

{----- END Types/data -----}


{----- Environment functions -----}
newEnvironment :: LLVMEnv
newEnvironment = LLVMEnv { functionName = "", parameters = [], returnType = TVoid, labelCounter = 1, regCounter = 0, environment = [empty], instructions = [] }

add :: LLVMInstr -> S ()
add i = do state <- get
           let prevInstr = instructions state
           put $ state { instructions = i:prevInstr }

addVar :: Id -> Type -> S Index
addVar x t = return 1

 
lookupVar :: Id -> S Index
lookupVar x = undefined

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

setFunctionParameters :: [(Type, Id)] -> S ()
setFunctionParameters typs = do state <- get
                                put $ state { parameters = typs }
                        
                
              
              
              
{----- END Environment functions -----}
