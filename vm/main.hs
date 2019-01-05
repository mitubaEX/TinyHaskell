import           Calls
import           Cell
import           Data.List          (isPrefixOf)
import           Data.List.Split    (splitOn)
import qualified Data.Map           as Map
import           Data.Text          (dropWhileEnd, pack, strip, unpack)
import           Eval
import           Functions
import           Parser
import           Replace
import           Run
import           System.Environment (getArgs)

main = do
  args <- getArgs
  fileBody <- if length args < 1
    then getContents
    else readFile $ head args
  -- print $ map (strip . pack) $ splitOn "-----------------------------\n" fileBody
  -- print $ map performTraverse (splitOn "-----------------------------\n" fileBody)
  let contents = splitOn "-----------------------------\n" fileBody
  let a = map performTraverse contents

  -- print $ map otherTraverse contents

  let valList = map performEval $ filter (/= Cell.Empty) a
  print valList

  let functions = functionList valList
  print functions

  let functionMap = list2Map functions
  print functionMap

  let calls = callsList valList
  print calls

  -- let resultList = performRun valList
  -- print resultList

  let result = otherRun functionMap calls
  print result

  -- let replaceReusltFunction = replaceFunction valList resultList
  -- print replaceReusltFunction
