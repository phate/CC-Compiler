import System.Environment (getArgs)
import System.Exit (exitFailure)

import Scanner
import Parser
import TypeChecker
import ErrM

import JVMGenerator
import JVMPrinter
import JVMEnv

check :: String -> IO () 
check s = let tree = parse $ alexScanTokens s in case typecheck tree of
  Ok (p,env) -> do putStrLn "TYPE CHECK OK, GENERATING JASMIN ASSEMBLY" 
                   let is = instr $ generateInstructions p
                   sequence_ [putStrLn i | i <- generateCode is]
                   return ()
  Bad err    -> do putStrLn "Type Error"
                   putStrLn err
                   return()
main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: Compiler <source>"
                         exitFailure
