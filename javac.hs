import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Cmd

import Scanner
import Parser
import TypeChecker
import ErrM

import JVMGenerator
import JVMPrinter
import JVMEnv

printEnv :: JVMEnv -> IO ()
printEnv e = do putStrLn $ "CurrStack: " ++ (show $ currentStackDepth e)
                putStrLn $ "MaxStack:  " ++ (show $ maxStackDepth e)
                putStrLn ""
                return ()

printEnvInfo :: [JVMEnv] -> IO ()
printEnvInfo envs = mapM_ printEnv envs
                         

check :: String -> IO ()
check s = let tree = parse $ alexScanTokens s in case typecheck tree of
  Ok (p,env) -> do putStrLn "TYPE CHECK OK, GENERATING JASMIN ASSEMBLY" 
                   let envs = generateInstructions p
                   let code = getCode "test" envs
		   printEnvInfo envs
		   writeFile "test.j" code
		   system "java -jar lib/jasmin.jar test.j"
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
