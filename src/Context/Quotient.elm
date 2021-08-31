module Context.Quotient exposing (..)

import AssocList as Dict exposing (Dict)
import Context exposing (Context)
import Data.Dict.Extra as Dict
import Maybe.Extra as Maybe
import Type exposing (Type)
import Value


type Quotient
    = Quotient { context : Context, substitutions : Type.Substitutions }


represent : Quotient -> Context
represent (Quotient { context, substitutions }) =
    Context.mapTypes
        (Type.represent substitutions)
        context


singleton : Value.Symbol -> Type -> Quotient
singleton symbol type_ =
    { context = Context.singleton symbol type_
    , substitutions = Dict.empty
    }
        |> Quotient


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
                (Type.mergeSubstitutions mergedContexts.substitutions mergedSubstitutions)
        )
        (Context.merge quotient1.context quotient2.context)
        (Type.mergeSubstitutions quotient1.substitutions quotient2.substitutions)
        |> Maybe.map Quotient
