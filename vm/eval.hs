module Eval (
    myEval,
    performEval
    ) where

import           CellTree
import           Control.Monad.State

data Value = Empty | Function String String

type Op = String

addM :: Num a => Maybe a -> Maybe a -> Maybe a
addM (Just a) (Just b) = Just $ a + b
addM _ _               = Just 0

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
myEval _ _ = ""

performEval :: Cell -> String
performEval a = myEval a ""
