import           CellTree
import           Data.List          (isPrefixOf)
import           Data.List.Split    (splitOn)
import           Data.Text          (dropWhileEnd, pack, strip, unpack)
import           Eval
import           Parser
import           System.Environment (getArgs)

main = do
  args <- getArgs
  fileBody <- if length args < 1
    then getContents
    else readFile $ head args
  print $ map (strip . pack) $ splitOn "-----------------------------\n" fileBody
  print $ map performTraverse (splitOn "-----------------------------\n" fileBody)
  let a = map performTraverse (splitOn "-----------------------------\n" fileBody)
  let valList = map performEval $ filter (/= CellTree.Empty) a
  print valList
