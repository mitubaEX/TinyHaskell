module Eval (
    myEval
    ) where

import           CellTree

type Op = String

myEval :: Cell -> Op -> String
myEval (Node a b) _
    | a == "=" = myEval b "="
    | a == "+" = myEval b a
    | a == "-" = myEval b a
    | a == "/" = myEval b a
    | a == "%" = myEval b a
myEval (Cons a b) c
    | c == "=" = emptyMyEval a ++ "=" ++ emptyMyEval b
    | c == "+" = show $ (read $ emptyMyEval a :: Integer) + (read $ emptyMyEval b :: Integer)
    | c == "-" = show $ (read $ emptyMyEval a :: Integer) - (read $ emptyMyEval b :: Integer)
    | c == "/" = show $ (read $ emptyMyEval a :: Integer) `div` (read $ emptyMyEval b :: Integer)
    | c == "%" = show $ (read $ emptyMyEval a :: Integer) `mod` (read $ emptyMyEval b :: Integer)
    | otherwise = emptyMyEval a ++ emptyMyEval b
    where emptyMyEval a = myEval a ""
myEval (Leaf a b) _ = b
