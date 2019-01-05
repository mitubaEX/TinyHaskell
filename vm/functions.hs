module Functions (
    functionList,
    list2Map,
    FunctionMap
    ) where

import qualified Data.Map as Map
import           Eval

type ValList = [Value]
type ResultList = [Value]
type FunctionMap = Map.Map Value Value

isFunction :: Value -> Bool
isFunction (Function _ _) = True
isFunction _              = False

functionList :: ValList -> [Value]
functionList = filter isFunction

myInsert :: Value -> FunctionMap -> FunctionMap
myInsert (Function a b) c = Map.insert a b c
myInsert _ _              = Map.empty

list2Map :: [Value] -> FunctionMap
list2Map = foldr myInsert Map.empty
