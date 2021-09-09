module Context.Quotient exposing (..)

import AssocList as Dict exposing (Dict)
import Context exposing (Context)
import Data.Dict.Extra as Dict
import Maybe.Extra as Maybe
import Type exposing (Type)
import Type.Substitutions exposing (Substitutions)
import Value


type Quotient
    = Quotient { context : Context, substitutions : Substitutions }


represent : Quotient -> Context
represent (Quotient { context, substitutions }) =
    Context.mapTypes
        (Type.Substitutions.represent substitutions)
        context


singleton : Value.Symbol -> Type -> Quotient
singleton symbol type_ =
    { context = Context.singleton symbol type_
    , substitutions = Dict.empty
    }
        |> Quotient


add : Value.Symbol -> Type -> Quotient -> Maybe Quotient
add valueSymbol type_ =
    merge
        (singleton valueSymbol type_)


merge : Quotient -> Quotient -> Maybe Quotient
merge (Quotient quotient1) (Quotient quotient2) =
    Maybe.andThen2
        (\mergedContexts mergedSubstitutions ->
            Maybe.map
                (\allMergedSubstitutions ->
                    { context = mergedContexts.context
                    , substitutions = allMergedSubstitutions
                    }
                )
                (Type.Substitutions.merge mergedContexts.substitutions mergedSubstitutions)
        )
        (Context.merge quotient1.context quotient2.context)
        (Type.Substitutions.merge quotient1.substitutions quotient2.substitutions)
        |> Maybe.map Quotient
