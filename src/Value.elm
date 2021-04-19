module Value exposing
    ( Symbol
    , Value
    , left
    , right
    , tuple
    , variable
    )

import Data.Symbol



-- Types


type Value
    = Variable Symbol
    | Left Value
    | Right Value
    | Tuple Value Value


type alias Symbol =
    Data.Symbol.Symbol ValueSymbolTag


type ValueSymbolTag
    = ValueSymbolTag



-- Helpers


variable =
    Variable


left =
    Left


right =
    Right


tuple =
    Tuple
