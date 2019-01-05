module Functions (
    functionList,
    list2Map
    ) where

import qualified Data.Map as Map
import           Eval

type ValList = [Value]
type ResultList = [Value]

isFunction :: Value -> Bool
isFunction (Function _ _) = True
isFunction _              = False

functionList :: ValList -> [Value]
functionList = filter isFunction

myInsert :: Value -> Map.Map Value Value -> Map.Map Value Value
myInsert (Function a b) c = Map.insert a b c
myInsert _ _              = Map.empty

list2Map :: [Value] -> Map.Map Value Value
list2Map = foldr myInsert Map.empty

