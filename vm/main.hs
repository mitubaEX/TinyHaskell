import           CellTree
import           Data.List          (isPrefixOf)
import           Data.List.Split    (splitOn)
import           Data.Text          (dropWhileEnd, pack, strip, unpack)
import           Eval
import           Parser
import           System.Environment (getArgs)

main = do
  args <- getArgs
  fileBody <- readFile $ head args
  print $ map (strip . pack) $ splitOn "-----------------------------\n" fileBody
  print $ map performTraverse (splitOn "-----------------------------\n" fileBody)
  let a = map performTraverse (splitOn "-----------------------------\n" fileBody)
  print $ map performEval $ filter (/= CellTree.Empty) a
