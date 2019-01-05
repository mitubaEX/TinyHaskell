module Calls (
    callsList
    ) where

import           Eval

type ValList = [Value]
type ResultList = [Value]

isCall :: Value -> Bool
isCall (Call _ _) = True
isCall (ID _)     = True
isCall _          = False

callsList :: ValList -> [Value]
callsList = filter isCall
