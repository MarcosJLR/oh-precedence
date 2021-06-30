module Precedence where

import qualified Data.Map.Strict as Map

type Token = String
type Symbol = String
type Rules = Map.Map Symbol Symbol

data Precedence
    = Equal Token Token
    | Less Token Token
    | Great Token Token
    deriving (Eq)
