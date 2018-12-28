module Parser (
    myTraverse,
    performTraverse,
    ) where

import           CellTree
import           Data.List       (isPrefixOf)
import           Data.List.Split (splitOn)
import           Data.Text       (dropWhileEnd, pack, strip, unpack)

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
