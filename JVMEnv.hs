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
type Context = Map Var Index
type Env = [Context]

data JVMEnv = JVMEnv 
    {
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
newEnv = JVMEnv { currentStackDepth = 0, maxStackDepth = 0, labelCounter = 0, env = [empty], instr = [] }

put :: [JVMInstr] -> S ()
put []     = return ()
put (x:xs) = do state <- get
                let instructions = instr state
                sPut $ state { instr = x:instructions }
                JVMEnv.put xs

addVar :: Var -> S Index
addVar x = do state <- get
              let (scope:rest) = env state
              let i = (1+) $ maximum $ ((-1):) $ concat [ elems i | i <- (scope:rest)]
              sPut $ state { env = ((insert x i scope):rest) }
              return i

lookupVar :: Var -> S Index
lookupVar x = do state <- get
                 let (scope:rest) = env state
                 lookupVar' (scope:rest) where
                   lookupVar' :: [Context] -> S Index
                   lookupVar' (c:cs) = case Data.Map.lookup x c of
                                         Nothing  -> lookupVar' cs
                                         Just i -> return i

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
                
              
              
              
{----- END Environment functions -----}
