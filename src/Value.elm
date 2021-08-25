module Value exposing
    ( Symbol
    , Value(..)
    , left
    , right
    , tuple
    , v1
    , v2
    , variable
    )

import Data.Symbol as Symbol



-- Types


type Value
    = Variable Symbol
    | Left Value
    | Right Value
    | Tuple Value Value


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



-- Testing


v1 : Value
v1 =
    tuple (left (variable "a")) (tuple (variable "b") (right (variable "c")))


v2 : Value
v2 =
    tuple (left (variable "a")) (tuple (variable "b") (right (variable "a")))
