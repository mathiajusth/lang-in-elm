module TypeAssignement exposing (typeOf)

import Type exposing (Type)
import Value exposing (Value)


type TypeAssignement
    = TypeAssignement Value Type


typeOf =
    TypeAssignement
