module Run (
    performRun,
    ValList,
    ResultList
    ) where

import qualified Data.Map  as Map
import           Eval
import           Functions

type ValList = [Value]
type ResultList = [Value]

unpackMaybeVal :: Maybe Value -> Value
unpackMaybeVal (Just a) = a
unpackMaybeVal _        = Eval.Empty

getValFromMap :: FunctionMap -> Value -> Value
getValFromMap a (ID b) = getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)

-- Add
getValFromMap a (Add (ID b) (ID c)) = Add (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))
getValFromMap a (Add (ID b) c) = Add (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) c
getValFromMap a (Add b (ID c)) = Add b (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))

-- Minus
getValFromMap a (Minus (ID b) (ID c)) = Minus (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))
getValFromMap a (Minus (ID b) c) = Minus (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) c
getValFromMap a (Minus b (ID c)) = Minus b (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))

-- Multi
getValFromMap a (Multi (ID b) (ID c)) = Multi (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))
getValFromMap a (Multi (ID b) c) = Multi (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) c
getValFromMap a (Multi b (ID c)) = Multi b (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))

-- Div
getValFromMap a (Div (ID b) (ID c)) = Div (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))
getValFromMap a (Div (ID b) c) = Div (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) c
getValFromMap a (Div b (ID c)) = Div b (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))

-- Mod
getValFromMap a (Mod (ID b) (ID c)) = Mod (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))
getValFromMap a (Mod (ID b) c) = Mod (getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)) c
getValFromMap a (Mod b (ID c)) = Mod b (getValFromMap a (unpackMaybeVal $ Map.lookup (ID c) a))

getValFromMap a b = b

performRun :: FunctionMap -> ValList -> ResultList
performRun a = map (getValFromMap a)
