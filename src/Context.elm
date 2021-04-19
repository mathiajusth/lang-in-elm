module Context exposing (..)

import AssocList as Dict exposing (Dict)
import Type exposing (Type)
import Value


type alias Context =
    Dict Value.Symbol Type


merge : Context -> Context -> Maybe Context
merge ctx1 ctx2 =
    -- unsafely (doesn't check for cycles)
    Dict.merge
        (\symbol type_ ->
            Maybe.map (Dict.insert symbol type_)
        )
        (\symbol type1 type2 ->
            Maybe.andThen
                (\accCtx ->
                    Maybe.map
                        (\type1Withtype2Subs ->
                            accCtx
                                |> Dict.insert symbol type1
                                |> Dict.map
                                    (\_ type_ ->
                                        type_
                                            |> Type.findQuotientRepresentative type1Withtype2Subs
                                    )
                        )
                        (Type.unify type1 type2)
                )
        )
        (\symbol type_ ->
            Maybe.map (Dict.insert symbol type_)
        )
        ctx1
        ctx2
        (Just Dict.empty)
