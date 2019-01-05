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

getValFromMap :: FunctionMap -> Value -> Maybe Value
getValFromMap a b = Map.lookup b a

otherRun :: FunctionMap -> ValList -> ResultList
otherRun a = map (unpackMaybeVal . getValFromMap a)
