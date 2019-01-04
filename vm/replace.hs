module Replace (
    replaceFunction
    ) where

import           Eval
import           Run

findDef :: ValList -> Value -> Value
findDef a (Pair (b, Args c)) = last $ findFunction a b
findDef a (Add (ID b) c)     = Add (last $ findFunction a (ID b)) c
findDef a (Add b (ID c))     = Add b (last $ findFunction a (ID c))
findDef a (Minus (ID b) c)   = Minus (last $ findFunction a (ID b)) c
findDef a (Minus b (ID c))   = Minus b (last $ findFunction a (ID c))
findDef a (Div (ID b) c)     = Div (last $ findFunction a (ID b)) c
findDef a (Div b (ID c))     = Div b (last $ findFunction a (ID c))
findDef a (Mod (ID b) c)     = Mod (last $ findFunction a (ID b)) c
findDef a (Mod b (ID c))     = Mod b (last $ findFunction a (ID c))
findDef a b                  = b

replaceFunction :: ValList -> ResultList -> ResultList
replaceFunction a = map (findDef a)
