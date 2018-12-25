import           CellTree
import           Data.List          (isPrefixOf)
import           Data.List.Split    (splitOn)
import           Data.Text          (dropWhileEnd, pack, strip, unpack)
import           System.Environment (getArgs)

myTraverse :: [String] -> Cell
myTraverse (x:y:xs)
  | "node" `isPrefixOf` x = Node (head . tail $ splitOn "(" x) (myTraverse $ y : xs)
  | "cons" `isPrefixOf` x = Cons (myTraverse [y]) (myTraverse xs)
  | "leaf" `isPrefixOf` x = Leaf (head . tail $ splitOn "(" $ head $ splitOn " " x) (unpack $ dropWhileEnd (==')') $ pack . head . tail $ splitOn " " x)
myTraverse [x]
  | "leaf" `isPrefixOf` x = Leaf (head . tail $ splitOn "(" $ head $ splitOn " " x) (unpack $ dropWhileEnd (==')') $ pack . head . tail $ splitOn " " x)
myTraverse _      =  Empty

main = do
  args <- getArgs
  fileBody <- readFile $ head args
  print $ map (strip . pack) $ splitOn "\n" fileBody
  print $ myTraverse $ map (unpack . strip . pack) (splitOn "\n" fileBody)
