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
import           SimpleParser
import           System.Environment (getArgs)

main = do
  args <- getArgs
  fileBody <- if length args < 1
    then getContents
    else readFile $ head args
  -- print $ map (strip . pack) $ splitOn "-----------------------------\n" fileBody
  -- print $ map performTraverse (splitOn "-----------------------------\n" fileBody)
  --
  -- let contents = splitOn "-----------------------------\n" fileBody

  let contents = splitOn "\n" fileBody
  let valList = map readExpr $ filter (not . null) contents
  --
  -- let a = map performTraverse $ filter (not . null) contents

  -- let valList = map performEval $ filter (/= Cell.Empty) a
  print valList

  let functions = functionList valList
  print functions

  let functionMap = list2Map functions
  print functionMap

  let calls = callsList valList
  print calls

  let result = performRun functionMap calls
  print result

  putStrLn "result:"
  mapM performPrint result
