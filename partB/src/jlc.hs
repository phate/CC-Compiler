import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix (dropFileName, takeFileName, splitExtension)
import System.Cmd
import System.IO

import Scanner
import Parser
import TypeChecker
import ErrM

import LLVMGenerator
import LLVMPrinter
import LLVMEnv
                         

check :: String -> String -> IO ()
check s file = let tree = parse $ alexScanTokens s in case typecheck tree of
  Ok (p,env) -> do
                  hPutStrLn stderr "OK"
                  let envs = generateInstructions p
                  let code = getCode envs
		  let assFile = file ++ ".ll"
		  let bitFile = file ++ ".bc"
                  writeFile assFile code
		  system $ "llvm-as -f " ++ assFile
		  system $ "llvm-ld " ++ bitFile ++  "  lib/runtime.bc -o " ++ file
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
            _      -> do putStrLn "Usage: jlc <source>"
                         exitFailure
