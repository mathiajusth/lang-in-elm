module Context exposing (Context, empty, mapTypes, merge, singleton)

import AssocList as Dict exposing (Dict)
import Data.Dict.Extra as Dict
import Data.Symbol as Symbol
import Maybe.Extra as Maybe
import Type exposing (Type)
import Type.Substitutions exposing (Substitutions)
import Value


type Context
    = Context Implementation


type alias Implementation =
    Dict Value.Symbol Type


empty : Context
empty =
    Dict.empty
        |> Context


singleton : Value.Symbol -> Type -> Context
singleton symbol type_ =
    Dict.empty
        |> Dict.insert symbol type_
        |> Context


mapTypes : (Type -> Type) -> Context -> Context
mapTypes mapType (Context context) =
    Dict.map (always mapType) context
        |> Context


merge : Context -> Context -> Maybe { context : Context, substitutions : Substitutions }
merge (Context context1) (Context context2) =
    Dict.binaryFallibleFold
        { initial =
            { context = Dict.empty
            , substitutions = Dict.empty
            }
        , onOneKeyMatch =
            \symbol type_ contextQuotient ->
                Just
                    { contextQuotient | context = Dict.insert symbol type_ contextQuotient.context }
        , onBothKeyMatch =
            \symbol type1 type2 contextQuotient ->
                Type.unify type1 type2
                    |> Maybe.andThen
                        (\type1Withtype2Substitutions ->
                            Type.Substitutions.merge type1Withtype2Substitutions contextQuotient.substitutions
                                |> Maybe.map
                                    (\mergedSubstitutions ->
                                        { context = contextQuotient.context |> Dict.insert symbol type1
                                        , substitutions = mergedSubstitutions
                                        }
                                    )
                        )
        }
        context1
        context2
        |> Maybe.map
            (\{ context, substitutions } ->
                { context = Context context
                , substitutions = substitutions
                }
            )
