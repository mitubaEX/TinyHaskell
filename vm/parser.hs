module Parser (
    myTraverse,
    performTraverse
    ) where

import           Cell
import           Control.Monad.State
import           Data.List           (isPrefixOf)
import           Data.List.Split     (splitOn)
import           Data.Text           (dropWhileEnd, pack, strip, unpack)

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

performTraverse :: String -> Cell
performTraverse a = myTraverse $ map (unpack . strip . pack) $ tail $ splitOn "\n" a

getNext :: State [String] ()
getNext = do
  s <- get
  if length s < 1 then put s else put (tail s)
  -- put (tail s)

-- traverse :: String -> State [String] Cell
-- traverse a = do
--   s <- get
--   if length s < 1
--      then Eval.Empty
--      else do
--        let a = head s
--        if "node" `isPrefixOf` a
--           then do
--             put (tail . tail $ s)
--             return (Node (head . tail $ splitOn "(" a) traverse)
--           else return (Leaf (head . tail . (splitOn "(") . head $ splitOn " " x) (unpack $ dropWhileEnd (==')') $ pack . unwords . tail $ splitOn " " x))
--
--
-- otherMyTraverse :: State [String] Cell
-- otherMyTraverse = do
--   s <- get
--   if length s < 1
--      then Eval.Empty
--      else traverse
--   -- getNext
--   -- return s
--
-- otherTraverse :: String -> [String]
-- otherTraverse a = evalState otherMyTraverse (map (unpack . strip . pack) $ tail $ splitOn "\n" a)
