module Value exposing
    ( Symbol
    , Value(..)
    , left
    , right
    , testing
    , toString
    , tuple
    , variable
    )

import Data.Symbol as Symbol



-- Types


type Value
    = Variable Symbol
      -- Sum
    | Left Value
    | Right Value
      -- Product
    | Tuple Value Value
    | ProjLeft Value
    | ProjRight Value
      -- Function
    | Lambda Symbol Value
    | Application Value Value


type alias Symbol =
    Symbol.Symbol ValueSymbolTag


type ValueSymbolTag
    = ValueSymbolTag



-- Helpers


toString : Value -> String
toString value =
    case value of
        Variable symbol ->
            Symbol.toString symbol

        Left leftValue ->
            "𝕃 " ++ toString leftValue

        Right rightValue ->
            "ℝ " ++ toString rightValue

        Tuple leftValue rightValue ->
            "(" ++ toString leftValue ++ " , " ++ toString rightValue ++ ")"

        ProjLeft innerValue ->
            "ℼᴸ " ++ toString innerValue

        ProjRight innerValue ->
            "ℼᴿ" ++ toString innerValue

        Lambda symbol body ->
            "(λ" ++ Symbol.toString symbol ++ ". " ++ toString body ++ ")"

        Application f x ->
            "[" ++ toString f ++ " _ " ++ toString x ++ "]"


variable : String -> Value
variable =
    Symbol.fromString >> Variable


left =
    Left


right =
    Right


tuple =
    Tuple


lambda =
    Lambda


app =
    Application



-- Testing


testing =
    { v1 =
        tuple (left (variable "a")) (tuple (variable "b") (right (variable "c")))
    , v2 =
        tuple (left (variable "a")) (tuple (variable "b") (right (variable "a")))
    , v3 =
        lambda (Symbol.fromString "x") (variable "0")
    , v4 =
        lambda (Symbol.fromString "x") (variable "x")
    , v5 =
        app
            (lambda
                (Symbol.fromString "x")
                (tuple (variable "x") (variable "x"))
            )
            (variable "y")
    , v6 =
        lambda (Symbol.fromString "f")
            (lambda
                (Symbol.fromString "x")
                (app (variable "f") (variable "x"))
            )
    , v61 =
        lambda
            (Symbol.fromString "x")
            (app (variable "f") (variable "x"))
    , v62 =
        app (variable "f") (variable "x")
    , v7 = ProjLeft (variable "x")
    , v8 = ProjLeft (tuple (variable "x") (variable "y"))
    , v9 = ProjLeft (left (variable "x"))
    }
