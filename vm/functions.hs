module Functions (
    functionList
    ) where

import           Eval

type ValList = [Value]
type ResultList = [Value]

isFunction :: Value -> Bool
isFunction (Function _ _) = True
isFunction _              = False

functionList :: ValList -> [Value]
functionList = filter isFunction
