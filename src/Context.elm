module Context exposing (..)

import AssocList as Dict exposing (Dict)
import Type exposing (Type)
import Value


type alias Context =
    Dict Value.Symbol Type


add : ValueVar -> Type -> Context -> Context
add valueVar type_ context =
    Debug.todo "TODO add"


unify : Context -> Context -> Maybe Context
unify ctx1 ctx2 =
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
                                        Type.findQuotientRepresentative type1Withtype2Subs type_
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



--
--
-- applyTypeSubstitutions : TypeSubstitutions -> Context -> Context
-- applyTypeSubstitutions typeSubstitutions context =
--     -- TODO this can cycle
--     Debug.todo "TODO applyTypeSubstitutions"


type alias ValueVar =
    String


type alias TypeVar =
    String
