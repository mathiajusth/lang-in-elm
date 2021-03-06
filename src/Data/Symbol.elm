module Data.Symbol exposing
    ( Symbol
    , compare
    , equals
    , fromString
    , toString
    )

import Data.Comparison as Comparison exposing (Comparison)


type Symbol a
    = Symbol String


equals : Symbol a -> Symbol a -> Bool
equals (Symbol string1) (Symbol string2) =
    string1 == string2


compare : Symbol a -> Symbol a -> Comparison
compare (Symbol string1) (Symbol string2) =
    if string1 == string2 then
        Comparison.Equal

    else if string1 > string2 then
        Comparison.Greater

    else
        Comparison.Lower


fromString : String -> Symbol a
fromString string =
    Symbol string


toString : Symbol a -> String
toString (Symbol string) =
    string
