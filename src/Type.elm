module Type exposing
    ( Type(..)
    , and
    , or
    , variable
    )

import AssocList as Dict exposing (Dict)
import Data.Comparison as Comparison
import Data.Set as Set exposing (Set)
import Data.Symbol as Symbol
import Maybe.Extra as Maybe



-- Types


type Type
    = Variable Symbol
    | Or Type Type
    | And Type Type


type alias Symbol =
    Symbol.Symbol TypeSymbolTag


type TypeSymbolTag
    = TypeSymbolTag


type alias Substitutions =
    Dict Symbol Type



-- Helpers


variable =
    Variable


or =
    Or


and =
    And


unify : Type -> Type -> Maybe Substitutions
unify type1 type2 =
    -- TODO try to write based on type constructor equality ~> if they are the same you match the correponding branches
    case ( type1, type2 ) of
        ( Variable typeSymbol1, Variable typeSymbol2 ) ->
            Just <|
                case Symbol.compare typeSymbol1 typeSymbol2 of
                    Comparison.Equal ->
                        Dict.empty

                    Comparison.Lower ->
                        Dict.singleton typeSymbol1 (Variable typeSymbol2)

                    Comparison.Greater ->
                        Dict.singleton typeSymbol2 (Variable typeSymbol1)

        ( Variable typeSymbol, type_ ) ->
            Just <|
                Dict.singleton typeSymbol type_

        ( type_, Variable typeSymbol ) ->
            Just <|
                Dict.singleton typeSymbol type_

        ( Or type1Left type1Right, Or type2Left type2Right ) ->
            Maybe.map2 mergeSubstitutions (unify type1Left type2Left) (unify type1Right type2Right)
                |> Maybe.join

        ( Or _ _, _ ) ->
            Nothing

        ( _, Or _ _ ) ->
            Nothing

        ( And type1Left type1Right, And type2Left type2Right ) ->
            Maybe.map2 mergeSubstitutions (unify type1Left type2Left) (unify type1Right type2Right)
                |> Maybe.join


mergeSubstitutions : Substitutions -> Substitutions -> Maybe Substitutions
mergeSubstitutions subs1 subs2 =
    Dict.merge
        (\symbol type_ ->
            Maybe.map (Dict.insert symbol type_)
        )
        (\symbol type1 type2 ->
            Maybe.andThen
                (\accSubs ->
                    Maybe.andThen
                        (\type1Withtype2Subs ->
                            mergeSubstitutions (Dict.insert symbol type1 type1Withtype2Subs) accSubs
                        )
                        (unify type1 type2)
                )
        )
        (\symbol type_ ->
            Maybe.map (Dict.insert symbol type_)
        )
        subs1
        subs2
        (Just Dict.empty)
