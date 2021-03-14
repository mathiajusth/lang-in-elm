module Value exposing (Value, left, right, tuple, var)


type Value
    = Var String
    | Left Value
    | Right Value
    | Tuple Value Value


var =
    Var


left =
    Left


right =
    Right


tuple =
    Tuple
