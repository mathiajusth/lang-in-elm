module Type.Substitutions exposing (..)

import AssocList as Dict exposing (Dict)
import Data.Dict.Extra as Dict
import Data.Symbol as Symbol
import List.Extra as List
import Type.Internal exposing (Type)


type alias Substitutions =
    Type.Internal.Substitutions


empty : Substitutions
empty =
    Dict.empty


realize : Substitutions -> (Type.Internal.Symbol -> Type)
realize substitutions =
    \symbol ->
        Dict.get symbol substitutions
            |> Maybe.withDefault (Type.Internal.Variable symbol)


represent : Substitutions -> Type -> Type
represent substitutions originalType =
    let
        shallowSubstitute : Substitutions -> (Type -> Type)
        shallowSubstitute =
            realize >> Type.Internal.expandLeaves
    in
    shallowSubstitute substitutions originalType
        |> (\newType ->
                if newType == originalType then
                    newType

                else
                    shallowSubstitute substitutions newType
           )


merge : Substitutions -> Substitutions -> Maybe Substitutions
merge =
    Type.Internal.mergeSubstitutions


normalize : Type -> Type
normalize type_ =
    let
        uniqueSumbols : List Type.Internal.Symbol
        uniqueSumbols =
            type_
                |> Type.Internal.toSymbolList
                |> List.map Symbol.toString
                -- TODO implement List.unique with for any type
                |> List.unique
                |> List.map Symbol.fromString

        simplifyingSubstitutions : Substitutions
        simplifyingSubstitutions =
            List.zip uniqueSumbols
                (List.range 1 (List.length uniqueSumbols)
                    |> List.map (String.fromInt >> Type.Internal.variableFromString)
                )
                |> Dict.fromList
    in
    represent simplifyingSubstitutions type_
