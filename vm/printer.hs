module Printer (
    performPrint
    ) where

import qualified Data.Map   as Map
import           Data.Maybe
import           Eval
import           Run

type ValList = [Value]
type ResultList = [Value]

performPrint :: Value -> IO ()
performPrint (ID a)      = print a
performPrint (Number a)  = print a
performPrint (Add a b)   = performPrint $ runOperator (Add a b)
performPrint (Minus a b) = performPrint $ runOperator (Minus a b)
performPrint (Multi a b) = performPrint $ runOperator (Multi a b)
performPrint (Div a b)   = performPrint $ runOperator (Div a b)
performPrint (Mod a b)   = performPrint $ runOperator (Mod a b)
performPrint a           = print a
