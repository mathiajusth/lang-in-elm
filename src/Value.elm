module Value exposing
    ( Symbol
    , Value(..)
    , left
    , right
    , testing
    , tuple
    , variable
    )

import Data.Symbol as Symbol



-- Types


type Value
    = Variable Symbol
    | Left Value
    | Right Value
    | Tuple Value Value
    | Lambda Symbol Value


type alias Symbol =
    Symbol.Symbol ValueSymbolTag


type ValueSymbolTag
    = ValueSymbolTag



-- Helpers


variable : String -> Value
variable =
    Symbol.fromString >> Variable


left =
    Left


right =
    Right


tuple =
    Tuple


labmda =
    Lambda



-- Testing


testing =
    { v1 =
        tuple (left (variable "a")) (tuple (variable "b") (right (variable "c")))
    , v2 =
        tuple (left (variable "a")) (tuple (variable "b") (right (variable "a")))
    , v3 =
        labmda (Symbol.fromString "x") (variable "0")
    , v4 =
        labmda (Symbol.fromString "x") (variable "x")
    }
