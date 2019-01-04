module Eval (
    myEval,
    performEval,
    Value(..)
    ) where

import           Cell
import           Control.Monad.State
import           Data.Char
import           Data.List.Split     (splitOn)

data Value =
    Empty
    | Function Value Value
    | Call Value Value
    | Add Value Value
    | Minus Value Value
    | Div Value Value
    | Mod Value Value
    | Number Integer
    | ID String
    | Pair (Value, Value)
    | Args [Value]
    | Other Value Value deriving (Show, Eq)

type Op = String

addM :: Num a => Maybe a -> Maybe a -> Maybe a
addM (Just a) (Just b) = Just $ a + b
addM _ _               = Just 0

myEval :: Cell -> Op -> Value
myEval (Node a b) _
    | a == "=" = myEval b "="
    | a == "CALL" = myEval b "CALL"
    | a == "+" = myEval b a
    | a == "-" = myEval b a
    | a == "/" = myEval b a
    | a == "%" = myEval b a
myEval (Cons a b) c
    | c == "=" = Function (emptyMyEval a) (emptyMyEval b)
    | c == "CALL" = Call (emptyMyEval a) (emptyMyEval b)
    | c == "+" = Add (emptyMyEval a) (emptyMyEval b)
    | c == "-" = Minus (emptyMyEval a) (emptyMyEval b)
    | c == "/" = Div (emptyMyEval a) (emptyMyEval b)
    | c == "%" = Mod (emptyMyEval a) (emptyMyEval b)
    | otherwise = Other (emptyMyEval a) (emptyMyEval b)
    where emptyMyEval a = myEval a ""
myEval (Leaf a b) _
    | all isDigit b = Number (read b :: Integer)
    | otherwise =
        if length splitedString == 1
           then ID b
           else Pair (ID (head splitedString), Args (map (\x -> Pair (ID x, Eval.Empty)) $ tail splitedString))
        where splitedString = splitOn " " b
myEval _ _ = ID ""

performEval :: Cell -> Value
performEval a = myEval a ""
