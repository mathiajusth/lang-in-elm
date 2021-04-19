module Main2 exposing (..)


type TypeSymbol
    = Type String


type ValueSymbol
    = Value String


set =
    [ Type "Zero"
    , Type "One"
    ]


one =
    Value "one"


isOfTypeOne value =
    case value of
        Value "one" ->
            True

        _ ->
            False


isOfTypeZero value =
    False
