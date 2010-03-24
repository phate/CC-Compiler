module TypeCheckerEnv where

import Control.Monad
import Control.Monad.State
import ErrM
import ParserAbs
import Data.Map

{----- Types/data -----}

type Env     = (Sig, [Context])
type Sig	= Map Id ([Type], Type)
type Context	= Map Id Type
	
type ErrStr  = String
type Types   = [Type]
type RetType = Type

data EnvState = EnvState 
    {
     retType :: RetType,  -- Return type for function currently being checked
     env :: Env
    }

type S a = StateT EnvState Err a
{----- END Types/data -----}


{----- Environment functions -----}
-- Creates a new environment
newEnv :: EnvState
newEnv = EnvState { retType = TVoid, env = (empty,[empty]) }

-- Makes the context in the current environment empty (called when checking a new function)
emptyContext :: S ()
emptyContext = do state <- get
                  let (sig, cont) = env state
                  put $ state { env = (sig, [empty]) }

-- Sets the return type of the current function being checked
setReturnType :: RetType -> S ()
setReturnType t = do state <- get
                     put $ state { retType = t }

-- Pushes a new scope on to the current context
pushScope :: S ()
pushScope = do state <- get
               let (sig, cont) = env state
               put $ state { env = (sig, empty:cont) }

-- Pops the top most scope from the current context
popScope :: S ()
popScope = do state <- get
              let (sig, (scope:rest)) = env state
              put $ state { env = (sig, rest) }

-- Adds a variable with its corresponding type to the current context
addVar :: Type -> Id -> S ()
addVar typ x =
  do state <- get
     let (sig, (scope:rest)) = env state
     case member x scope of
       False -> put $ state { env = (sig, ((insert x typ scope):rest)) }
       True  -> fail $ "Variable " ++ x ++ " was already declared in this scope"

-- Returns the type of a variable in the current context
lookupVar :: Id -> S Type
lookupVar x = do state <- get
                 let (sig, (scope:rest)) = env state
                 lookupVar' (scope:rest) where
                   lookupVar' :: [Context] -> S Type
                   lookupVar' []     = fail $ "VarNotFound"
                   lookupVar' (c:cs) = case Data.Map.lookup x c of
                                         Nothing  -> lookupVar' cs
                                         Just typ -> return typ

-- Adds a function with its corresponding return type & parameter types, to the current signature
addFun :: Id -> ([Type], Type) -> S ()
addFun id typs = do state <- get
                    let (sig, cont) = env state
                    case member id sig of
                      True  -> fail $ "AddFun"
                      False -> put $ state { env = (insert id typs sig, cont) }

-- Returns the return type & parameter types of a function in the current signature
lookupFun :: Id -> S ([Type], Type)
lookupFun id = do state <- get
                  let (sig, const) = env state
                  case Data.Map.lookup id sig of
                    Nothing  -> fail $ "FunNotFound"
                    Just ret -> return ret
{----- END Environment functions -----}
