module CellTree (
    Cell(..)
    ) where

data Cell = Leaf String String | Cons Cell Cell | Node String Cell | Empty deriving (Show, Eq)
