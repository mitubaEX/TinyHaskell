module Replace (
    replaceFunction
    ) where

import           Eval
import           Run

findDef :: ValList -> Value -> Value
findDef a (Pair (b, Args c)) = last $ findFunction a b
findDef a (Add (ID b) c)     = Add (last $ findFunction a (ID b)) c
findDef a (Add b (ID c))     = Add b (last $ findFunction a (ID c))
findDef a b                  = b

replaceFunction :: ValList -> ResultList -> ResultList
replaceFunction a = map (findDef a)
