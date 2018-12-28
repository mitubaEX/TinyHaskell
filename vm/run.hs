module Run (
    myRun,
    performRun,
    ) where

import           Eval

eqID :: Value -> Value -> Bool
eqID (Function (ID a) _) (ID b) = a == b
eqID _ _                        = False

getVal :: Value -> Value
getVal (Function a b) = b
getVal _              = ID ""

filterValues :: (Value -> Bool) -> [Value] -> [Value]
filterValues a (x:xs)
    | a x = getVal x : filterValues a xs
    | otherwise = filterValues a xs
filterValues a [] = []

findFunction :: [Value] -> Value -> [Value]
findFunction a b = filterValues (`eqID` b) a

myRun :: [Value] -> Value -> [Value]
myRun = findFunction

performRun :: [Value] -> [Value]
performRun a = map last $ filter (not . null) $ map (myRun a) a
