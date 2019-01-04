module Replace (
    replaceFunction
    ) where

import           Eval
import           Run

findDef :: ValList -> Value -> Value
findDef a (Pair (b, Args c)) = last $ findFunction a b
findDef a (Add b c)          = Add (last $ findFunction a b) c

replaceFunction :: ValList -> ResultList -> ResultList
replaceFunction a = map (findDef a)
