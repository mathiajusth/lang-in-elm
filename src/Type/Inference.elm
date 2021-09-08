module Type.Inference exposing (..)

import Context exposing (Context)
import Type exposing (Type)
import Type.Substitutions exposing (Substitutions)


type alias State =
    { ctx : Context
    , subs : Substitutions
    , varsCount : Int
    }


type alias Inference =
    State -> ( State, Type )


emptyState : State
emptyState =
    { ctx = Context.empty
    , subs = Type.Substitutions.empty
    , varsCount = 0
    }



-- pure : a ->  Inference a
-- pure =
