module Run (
    performRun,
    ValList,
    ResultList,
    runOperator
    ) where

import qualified Data.Map   as Map
import           Data.Maybe
import           Eval
import           Functions

type ValList = [Value]
type ResultList = [Value]

unpackMaybeVal :: Maybe Value -> Value
unpackMaybeVal (Just a) = a
unpackMaybeVal _        = Eval.Empty

runOperator :: Value -> Value
runOperator (Add (Number a) (Number b))   = Number (a + b)
runOperator (Add a (Number b))            = runOperator $ Add (runOperator a) (Number b)
runOperator (Add (Number a) b)            = runOperator $ Add (Number a) (runOperator b)

runOperator (Minus (Number a) (Number b)) = Number (a - b)
runOperator (Minus a (Number b))            = runOperator $ Minus (runOperator a) (Number b)
runOperator (Minus (Number a) b)            = runOperator $ Minus (Number a) (runOperator b)

runOperator (Multi (Number a) (Number b)) = Number (a * b)
runOperator (Multi a (Number b))            = runOperator $ Multi (runOperator a) (Number b)
runOperator (Multi (Number a) b)            = runOperator $ Multi (Number a) (runOperator b)

runOperator (Div (Number a) (Number b))   = Number (a `div` b)
runOperator (Div a (Number b))            = runOperator $ Div (runOperator a) (Number b)
runOperator (Div (Number a) b)            = runOperator $ Div (Number a) (runOperator b)

runOperator (Mod (Number a) (Number b))   = Number (a `mod` b)
runOperator (Mod a (Number b))            = runOperator $ Mod (runOperator a) (Number b)
runOperator (Mod (Number a) b)            = runOperator $ Mod (Number a) (runOperator b)
runOperator a                             = a

-- (Number or ID) -> (ID) -> Value
replaceIDVal :: FunctionMap -> Value -> Value -> Value -> Value
-- ID
-- TODO: ID local or global scope
replaceIDVal _ (Number a) (ID b) (ID c)
  | b == c = Number a
  | otherwise = ID c

-- Call
replaceIDVal fs a b (Call c d) = getValFromMap fs (Call c (runOperator $ replaceIDVal fs a b d))

-- Each operator
replaceIDVal fs a b (Add c d) = Add (replaceIDVal fs a b c) (replaceIDVal fs a b d)
replaceIDVal fs a b (Minus c d) = Minus (replaceIDVal fs a b c) (replaceIDVal fs a b d)
replaceIDVal fs a b (Multi c d) = Multi (replaceIDVal fs a b c) (replaceIDVal fs a b d)
replaceIDVal fs a b (Div c d) = Div (replaceIDVal fs a b c) (replaceIDVal fs a b d)
replaceIDVal fs a b (Mod c d) = Mod (replaceIDVal fs a b c) (replaceIDVal fs a b d)
replaceIDVal fs a b c = c

-- Call(Pair) -> Key(Pair) -> Value with Key
applyArgs :: FunctionMap -> Value -> Value -> Value -> Value
applyArgs fs (Pair (ID a, Args (x:xs))) (Pair (ID c, Args (y:ys))) e =
    applyArgs fs (Pair (ID a, Args xs)) (Pair (ID c, Args ys)) replacedVal
        where replacedVal = replaceIDVal fs x y e
applyArgs _ (Pair (ID a, Args [])) (Pair (ID c, Args [])) e = e

isNumber :: Value -> Bool
isNumber (Number a) = False
isNumber _          = True

filterKeys :: Value -> Value -> Bool
filterKeys (Pair (ID a, Args b)) (Pair (ID c, Args d)) = a == c && length b == length d && all isNumber d
filterKeys _ _ = False

-- Get keys of FunctionMap and apply args to function.
getSameArgsFunction :: FunctionMap -> Value -> Value
getSameArgsFunction a b =
    if not (null filteredKeyList)
       then applyArgs a b filteredKey (unpackMaybeVal $ Map.lookup filteredKey a)
       else Eval.Empty
           where filteredKeyList = filter (filterKeys b) $ Map.keys a
                 filteredKey = head filteredKeyList

-- Get val from map with recursive.
-- And return ResultValue
getValFromMap :: FunctionMap -> Value -> Value
getValFromMap a (ID b) = getValFromMap a (unpackMaybeVal $ Map.lookup (ID b) a)

-- Call
getValFromMap a (Call (ID b) (ID c)) = getValFromMap a (getSameArgsFunction a (Pair (ID b, Args [ID c])))
getValFromMap a (Call (ID b) (Number c))
  | isJust lookupedVal = unpackMaybeVal lookupedVal
  | otherwise = getValFromMap a (getSameArgsFunction a (Pair (ID b, Args [Number c])))
  where lookupedVal = Map.lookup (Pair (ID b, Args [Number c])) a
getValFromMap a (Call (ID b) (Args c)) = getValFromMap a (getSameArgsFunction a (Pair (ID b, Args c)))

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
