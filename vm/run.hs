module Run (
    performRun,
    findFunction,
    ValList,
    ResultList,
    otherRun
    ) where

import qualified Data.Map  as Map
import           Eval
import           Functions

type ValList = [Value]
type ResultList = [Value]

eqID :: Value -> Value -> Bool
eqID (Function (ID a) _) (ID b) = a == b
eqID _ _                        = False

isID :: Value -> Bool
isID (ID a) = True
isID _      = False

getVal :: Value -> Value
getVal (Function a b) = b
getVal _              = ID ""

filterValues :: (Value -> Bool) -> [Value] -> [Value] -> [Value]
filterValues a (x:xs) xss
    | a x =
        if isID $ getVal x
           then filterValues (`eqID` getVal x) xss xss ++ filterValues a xs xss
           else getVal x : filterValues a xs xss
    | otherwise = filterValues a xs xss
filterValues a [] _ = []

findFunction :: ValList -> Value -> ResultList
findFunction a b = filterValues (`eqID` b) a a

performRun :: ValList -> ResultList
performRun a = map last $ filter (not . null) $ map (findFunction a) a

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

otherRun :: FunctionMap -> ValList -> ResultList
otherRun a = map (getValFromMap a)
