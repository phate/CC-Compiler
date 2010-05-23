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

import Desugarer
                         

check :: String -> String -> IO ()
check s file = let tree = parse $ alexScanTokens s in
    case typecheck tree of
    Ok (p,tcenv) -> do
                    hPutStrLn stderr "OK"
                    putStrLn (show p)
                    putStrLn (show $ desugar p tcenv)
                    let llvmenv = generateInstructions (desugar p tcenv)
                    --let llvmenv = generateInstructions p
                    let code = getCode llvmenv
                    writeFile (file ++ ".ll") code
                    system $ "llvm-as -f " ++ file ++ ".ll" ++ "&& llvm-ld lib/runtime.bc " ++ file ++ ".bc"
                    --system $ "java -jar lib/jasmin.jar " ++ file ++ ".j" ++ " && mv " ++ (takeFileName file) ++ ".class " ++ file ++ ".class" 
                    return ()
    Bad err    -> do
                    hPutStrLn stderr "ERROR"
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
