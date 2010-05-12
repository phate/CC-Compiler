module TypeCheckerEnv where

import Control.Monad
import Control.Monad.State
import ErrM
import ParserAbs
import Data.Map

{----- Types/data -----}

type Env      = (Sig, [Context])
type Sig	    = Map Id ([DType], DType)
type Context	= Map Id DType

type Struct     = [(Id, DType)]
type Structs    = Map Id Struct
type PtrTypeDefs = Map Id Id -- Maps a type defined pointer to the corresponding struct

type Classes    = Map Id [DType] -- CHANGE THIS LATER
	
type ErrStr  = String
type Types   = [DType]
type RetType = DType


data TCFctEnv = TCFctEnv
  {
    retType :: RetType,
    env :: Env
  }

newTCFctEnv :: TCFctEnv
newTCFctEnv = TCFctEnv { retType = TVoid, env = (addPredefFuns empty,[empty]) }

addPredefFuns :: Sig -> Sig
addPredefFuns sig   = insert "printInt" ([DType TInt 0], TVoid)
                    $ insert "printDouble" ([DType TDouble 0], TVoid)
                    $ insert "printString" ([TString], TVoid)
                    $ insert "readInt" ([], DType TInt 0)
                    $ insert "readDouble" ([], DType TDouble 0) sig                     

data TCEnv = TCEnv
  {
    tcFctEnv :: TCFctEnv,
    structs :: Structs,
    ptrTypeDefs :: PtrTypeDefs,
    classes :: Classes  
  }

type S a = StateT TCEnv Err a

newTCEnv :: TCEnv
newTCEnv = TCEnv { tcFctEnv = newTCFctEnv, structs = empty, ptrTypeDefs = empty, classes = empty }

lookupFun :: Id -> S ([DType], DType)
lookupFun id = do s <- get
                  let (sig, const) = env $ tcFctEnv s
                  case Data.Map.lookup id sig of
                    Nothing  -> fail $ "Function " ++ id ++ " not found"
                    Just ret -> return ret

emptyContext :: S ()
emptyContext = do s <- get
                  let (sig, _) = env $ tcFctEnv s
                  put $ s { tcFctEnv = (tcFctEnv s) { env = (sig,[empty]) } }

getReturnType :: S RetType
getReturnType = do  s <- get
                    return $ retType $ tcFctEnv s

setReturnType :: RetType -> S ()
setReturnType t = do s <- get
                     put $ s { tcFctEnv = (tcFctEnv s) { retType = t } }

pushScope :: S ()
pushScope = do s <- get
               let (sig, cont) = env $ tcFctEnv s
               put $ s { tcFctEnv = (tcFctEnv s) { env = (sig, empty:cont) } }

popScope :: S ()
popScope = do s <- get
              let (sig, (scope:rest)) = env $ tcFctEnv s
              put $ s { tcFctEnv = (tcFctEnv s) { env = (sig, rest) } }

addVar :: DType -> Id -> S ()
addVar typ x =
  do s <- get
     let (sig, (scope:rest)) = env $ tcFctEnv s
     case member x scope of
       False -> put $ s { tcFctEnv = (tcFctEnv s) { env = (sig, ((insert x typ scope):rest)) } }
       True  -> fail $ "Variable " ++ x ++ " was already declared in this scope"

lookupVar :: Id -> S DType
lookupVar x = do s <- get
                 let (sig, (scope:rest)) = env $ tcFctEnv s
                 lookupVar' (scope:rest) where
                   lookupVar' :: [Context] -> S DType
                   lookupVar' []     = fail $ "Variable " ++ x ++ " not found"
                   lookupVar' (c:cs) = case Data.Map.lookup x c of
                                         Nothing  -> lookupVar' cs
                                         Just typ -> return typ

addFun :: Id -> ([DType], DType) -> S ()
addFun id typs = do s <- get
                    let (sig, cont) = env $ tcFctEnv s
                    case member id sig of
                      True  -> fail $ "Function " ++ id ++ " multiply declared"
                      False -> put $ s { tcFctEnv = (tcFctEnv s) { env = (insert id typs sig, cont) } }


-- This function is used for ENew
lookupStructClass :: Id -> S DType
lookupStructClass id = do s <- get
                          let strDefs = structs s
                          let classDefs = classes s
                          case Data.Map.lookup id strDefs of
                            Just s  -> return (TIdent id)
                            Nothing -> case Data.Map.lookup id classDefs of 
                                         Just c  -> return (TIdent id)
                                         Nothing -> fail $ id ++ ": No such type defined"

-- This function takes a pointer typedef and returns the struct
lookupPointer :: Id -> S DType
lookupPointer id = do s <- get
                      let ptrDefs = ptrTypeDefs s
                      let classDefs = classes s
                      case Data.Map.lookup id ptrDefs of
                         Just s  -> return (TIdent s)
                         Nothing -> case Data.Map.lookup id classDefs of
                                      Just c  -> return (TIdent id)
                                      Nothing -> fail $ id ++ ": No such type defined"

-- Returns the type of a field in a struct
lookupField :: Id -> Id -> S DType
lookupField id field = 
  do s <- get
     let strDefs = structs s
     case Data.Map.lookup id strDefs of
       Nothing -> fail $ id ++ ": No such struct exists"
       Just s  -> case Prelude.lookup field s of
         Nothing -> fail $ "Field " ++ field ++ " not found in " ++ id
         Just t  -> return t

-- Adds a struct to "structs" in environment
addStruct :: Id -> [(Id, DType)] -> S ()
addStruct id decls = do s <- get
                        let strDefs = structs s
                        case member id strDefs of
                          True -> fail $ "Structure " ++ id ++ " multiply declared"
                          False -> put $ s { structs = (insert id decls strDefs) }

-- Adds a pointer type definition to the environment
addPtrTypeDef :: Id -> Id -> S ()
addPtrTypeDef str ptr = 
  do s <- get
     let ptrDefs = ptrTypeDefs s
     let strDefs = structs s
     case member str strDefs of
       True -> put $ s { ptrTypeDefs = (insert ptr str ptrDefs) }
       False -> fail $ "Can't create pointer type definition \"" ++
                         ptr ++ "\" to undefined struct \"" ++ str ++ "\""
