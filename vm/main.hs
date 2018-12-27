import           CellTree
import           Data.List          (isPrefixOf)
import           Data.List.Split    (splitOn)
import           Data.Text          (dropWhileEnd, pack, strip, unpack)
import           Eval
import           System.Environment (getArgs)

myTraverse :: [String] -> Cell
myTraverse (x:y:xs)
  | "node" `isPrefixOf` x = Node (secondElm $ splitedByRParen x) (myTraverse $ y : xs)
  | "cons" `isPrefixOf` x = Cons (myTraverse [y]) (myTraverse xs)
  | "leaf" `isPrefixOf` x = Leaf (secondElm . splitedByRParen . head $ splitedBySpace x) (unpack $ dropWhileEnd (==')') $ pack . unwords . tail $ splitedBySpace x)
  where splitedBySpace = splitOn " "
        splitedByRParen = splitOn "("
        secondElm = head . tail
myTraverse [x]
  | "leaf" `isPrefixOf` x = Leaf (head . tail $ splitOn "(" $ head $ splitOn " " x) (unpack $ dropWhileEnd (==')') $ pack . unwords . tail $ splitOn " " x)
myTraverse _      =  Empty

main = do
  args <- getArgs
  fileBody <- readFile $ head args
  print $ map (strip . pack) $ splitOn "\n" fileBody
  print $ myTraverse $ map (unpack . strip . pack) (splitOn "\n" fileBody)
  print $ myEval (myTraverse $ map (unpack . strip . pack) (splitOn "\n" fileBody)) ""
