import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix (dropFileName, takeFileName, splitExtension)
import System.Cmd
import System.IO

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
                         

check :: String -> String -> IO ()
check s file = let tree = parse $ alexScanTokens s in case typecheck tree of
  Ok (p,env) -> do
                  hPutStrLn stderr "OK"
                  let envs = generateInstructions p
                  let code = getCode (takeFileName file) envs
                  printEnvInfo envs
                  writeFile (file ++ ".j") code
                  system $ "java -jar lib/jasmin.jar " ++ file ++ ".j" ++ " && mv " ++ (takeFileName file) ++ ".class " ++ file ++ ".class" 
                  return ()
  Bad err    -> do hPutStrLn stderr "ERROR"
                   putStrLn err
                   return()

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
                        let (file',ext) = splitExtension file
                        case ext of
                          ".jl" -> do c <- readFile file
                                      check c file'
                          _     -> do putStrLn "No *.jl file as input"
                                      exitFailure  
            _      -> do putStrLn "Usage: Compiler <source>"
                         exitFailure
