module JVMEnv where

import Control.Monad
import Control.Monad.State
import Data.Map
import ParserAbs
import JVMAbs

{----- Types/data -----}
type Label = String
type Var = Id
type Index = Integer
type Context = Map Var (Index,Type)
type Env = [Context]

data JVMEnv = JVMEnv 
    {
     funName :: Id,
     params :: [Type],
     returnType :: Type,
     currentStackDepth :: Integer,
     maxStackDepth :: Integer,
     labelCounter :: Integer,
     env :: Env,
     instr :: [JVMInstr]
    } 
    deriving Show

type S a = State JVMEnv a

sPut = Control.Monad.State.put
{----- END Types/data -----}


{----- Environment functions -----}
newEnv :: JVMEnv
newEnv = JVMEnv { funName = "", params = [], returnType = TVoid, currentStackDepth = 0, maxStackDepth = 0, labelCounter = 0, env = [empty], instr = [] }

put :: [JVMInstr] -> S ()
put []     = return ()
put (x:xs) = do state <- get
                let instructions = instr state
                sPut $ state { instr = x:instructions }
                JVMEnv.put xs

addVar :: Var -> Type -> S Index
addVar x t = do state <- get
                let (scope:rest) = env state
                let vars = concat [elems i | i <- (scope:rest)]
                let (index,t') = foldr f (-1,TInt) vars
                let index' = if t' == TDouble then index+2 else index+1
                sPut $ state { env = ((insert x (index',t) scope):rest) }
                return index'
  where f = \(i,t') (f,t'') -> if i == max i f then (i,t') else (f,t'')

lookupVar :: Var -> S Index
lookupVar x = do state <- get
                 let (scope:rest) = env state
                 lookupVar' (scope:rest) where
                   lookupVar' :: [Context] -> S Index
                   lookupVar' (c:cs) = case Data.Map.lookup x c of
                                         Nothing     -> lookupVar' cs
                                         Just (i,t)  -> return i

getLabel :: S LabelStr
getLabel = do state <- get
              let count = labelCounter state
              let label = "lab" ++ (show count)
              sPut $ state { labelCounter = count + 1 }
              return label

pushScope :: S ()
pushScope = do state <- get
               let cont = env state
               sPut $ state { env = (empty:cont) }

popScope :: S ()
popScope = do state <- get
              let (scope:rest) = env state
              sPut $ state { env = (rest) }

incStack :: Integer -> S ()
incStack i = do state <- get
                let curr = i + currentStackDepth state
                let max = maxStackDepth state
                let newMax = if curr > max then curr else max
                sPut $ state { currentStackDepth = curr, maxStackDepth = newMax }
                
getReturnType :: S Type
getReturnType = do state <- get
                   return (returnType state)
                   
setReturnType :: Type -> S ()
setReturnType t = do state <- get
                     sPut $ state { returnType = t }
                     
setFunctionName :: Id -> S ()
setFunctionName name = do state <- get
                          sPut $ state { funName = name }

setParameters :: [Type] -> S ()
setParameters typs = do state <- get
                        sPut $ state { params = typs }
                
              
              
              
{----- END Environment functions -----}
