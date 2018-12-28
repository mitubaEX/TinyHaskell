module Eval (
    myEval,
    performEval,
    Value(..)
    ) where

import           CellTree
import           Control.Monad.State
import           Data.Char

data Value =
    Empty
    | Function Value Value
    | Add Value Value
    | Minus Value Value
    | Div Value Value
    | Mod Value Value
    | Number Integer
    | ID String
    | Other Value Value deriving (Show, Eq)

type Op = String

addM :: Num a => Maybe a -> Maybe a -> Maybe a
addM (Just a) (Just b) = Just $ a + b
addM _ _               = Just 0

myEval :: Cell -> Op -> Value
myEval (Node a b) _
    | a == "=" = myEval b "="
    | a == "+" = myEval b a
    | a == "-" = myEval b a
    | a == "/" = myEval b a
    | a == "%" = myEval b a
myEval (Cons a b) c
    | c == "=" = Function (emptyMyEval a) (emptyMyEval b)
    | c == "+" = Add (emptyMyEval a) (emptyMyEval b)
    | c == "-" = Minus (emptyMyEval a) (emptyMyEval b)
    | c == "/" = Div (emptyMyEval a) (emptyMyEval b)
    | c == "%" = Mod (emptyMyEval a) (emptyMyEval b)
    | otherwise = Other (emptyMyEval a) (emptyMyEval b)
    where emptyMyEval a = myEval a ""
myEval (Leaf a b) _
    | all isDigit b = Number (read b :: Integer)
    | otherwise = ID b
myEval _ _ = ID ""

performEval :: Cell -> Value
performEval a = myEval a ""
