module Type exposing (Type(..), and, or, var)


type Type
    = Var String
    | Or Type Type
    | And Type Type


var =
    Var


or =
    Or


and =
    And
