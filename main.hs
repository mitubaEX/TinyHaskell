import           Calls
import           Cell
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

main = do
  args <- getArgs
  fileBody <- if length args < 1
    then getContents
    else readFile $ head args

  let contents = splitOn "\n" fileBody
  let valList = map readExpr $ filter (not . null) contents
  -- print valList

  let functions = functionList valList
  -- print functions

  let functionMap = list2Map functions
  -- print functionMap

  let calls = callsList valList
  -- print calls

  let result = performRun functionMap calls
  -- print result

  putStrLn "result:"
  mapM performPrint result
