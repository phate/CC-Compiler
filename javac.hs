import System.Environment (getArgs)
import System.Exit (exitFailure)

import Scanner
import Parser
import TypeChecker
import ErrM

--import JVMGenerator

check :: String -> IO () 
check s = let tree = parse $ alexScanTokens s
					in case typecheck tree of
						Ok (p,env) 	-> do
													print $ show p
													return()
						Bad err 		-> do
													putStrLn "Type Error"
													putStrLn err
													return()
main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: Compiler <source>"
                         exitFailure
