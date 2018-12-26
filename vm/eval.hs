module Eval (
    myEval
    ) where

import           CellTree

type Op = String

myEval :: Cell -> Op -> String
myEval (Node a b) _
    | a == "=" = myEval b "="
    | a == "+" = myEval b a
myEval (Cons a b) c
    | c == "=" = emptyMyEval a ++ "=" ++ emptyMyEval b
    | c == "+" = emptyMyEval a ++ "+" ++ emptyMyEval b
    | otherwise = emptyMyEval a ++ emptyMyEval b
    where emptyMyEval a = myEval a ""
myEval (Leaf a b) _ = b
