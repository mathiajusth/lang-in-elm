module TypeAssignement exposing (typeOf)

import Type exposing (Type)
import Value exposing (Value)


type TypeAssignement
    = ValueOfType Value Type


typeOf =
    ValueOfType
