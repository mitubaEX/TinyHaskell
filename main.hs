import           Calls
import           Cell
import           Data.IORef
import           Data.List          (isPrefixOf)
import           Data.List.Split    (splitOn)
import qualified Data.Map           as Map
import           Data.Text          (dropWhileEnd, pack, strip, unpack)
import           Eval
import           Functions
import           Parser
import           Printer
import           Run
import           System.Environment (getArgs)
import           System.IO
import           System.IO.Unsafe

{-# NOINLINE funcs #-}
funcs :: IORef FunctionMap
funcs = unsafePerformIO $ newIORef Map.empty

runRepl :: IO ()
runRepl = do
  putStr "REPL >>> "
  hFlush stdout
  fileBody <- getLine
  runOnce fileBody
  runRepl

runOnce :: String -> IO ()
runOnce fileBody = do
  let contents = splitOn "\n" fileBody

  let valList = map readExpr $ filter (not . null) contents
  -- print valList

  let functions = functionList valList
  -- print functions

  let functionMap = list2Map functions
  -- print functionMap
  atomicModifyIORef funcs (\m -> (Map.union functionMap m, ()))
  fs <- readIORef funcs

  let calls = callsList valList
  -- print calls

  let result = performRun fs calls
  -- print result

  mapM_ performPrint result


main = do
  args <- getArgs
  if length args < 1
    then runRepl
    else do
      fileBody <- readFile $ head args
      runOnce fileBody
