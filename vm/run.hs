module Run (
    myRun,
    performRun,
    findFunction,
    ValList,
    ResultList
    ) where

import           Eval

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

filterValues :: (Value -> Bool) -> [Value] -> [Value]
filterValues a (x:xs)
    | a x = if isID $ getVal x then filterValues (`eqID` getVal x) xs ++ filterValues a xs else getVal x : filterValues a xs
    | otherwise = filterValues a xs
filterValues a [] = []

findFunction :: ValList -> Value -> ResultList
findFunction a b = filterValues (`eqID` b) a

myRun :: [Value] -> Value -> [Value]
myRun = findFunction

performRun :: ValList -> ResultList
performRun a = map last $ filter (not . null) $ map (findFunction a) a
