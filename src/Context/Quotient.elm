-- make opaque


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


representType : Quotient -> Type -> Type
representType (Quotient quotient) type_ =
    Type.Substitutions.represent quotient.substitutions type_


empty : Quotient
empty =
    { context = Context.empty
    , substitutions = Dict.empty
    }
        |> Quotient


singleton : Value.Symbol -> Type -> Quotient
singleton symbol type_ =
    { context = Context.singleton symbol type_
    , substitutions = Dict.empty
    }
        |> Quotient


get : Value.Symbol -> Quotient -> Maybe Type
get valueSymbol (Quotient { context }) =
    Context.get valueSymbol context


type Error
    = DummyError


add : Value.Symbol -> Type -> Quotient -> Result Error Quotient
add valueSymbol type_ =
    merge
        (singleton valueSymbol type_)
        >> Result.fromMaybe DummyError


remove : Value.Symbol -> Quotient -> Quotient
remove valueSymbol (Quotient quotient) =
    Quotient
        { quotient | context = Context.remove valueSymbol quotient.context }


addEq : Type -> Type -> Quotient -> Result Error Quotient
addEq t1 t2 (Quotient quotient) =
    Type.unify t1 t2
        |> Maybe.andThen
            (\newSubs ->
                Type.Substitutions.merge newSubs quotient.substitutions
                    |> Maybe.map
                        (\mergedSubs ->
                            Quotient { quotient | substitutions = mergedSubs }
                        )
            )
        |> Result.fromMaybe DummyError


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
