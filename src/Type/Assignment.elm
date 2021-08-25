module Type.Assignement exposing (typeOf)

import Type exposing (Type)
import Value exposing (Value)


type Assignement
    = Assignement Value Type


typeOf =
    Assignement
