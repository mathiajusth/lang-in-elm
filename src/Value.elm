module Value exposing
    ( Symbol
    , Value
    , left
    , right
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
