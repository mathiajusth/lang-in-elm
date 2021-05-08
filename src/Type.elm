module Type exposing
    ( Type(..), variable, and, or, unify, represent
    , a1ANDa2_ORb2, a3ANDa4_ORb, aORb, mergeSubstitutions, u1, u2, t
    , Substitutions
    )

{-| tralala.


# Export

@docs Type, variable, and, or, unify, represent, Substitution


# Testing

@docs a1ANDa2_ORb2, a3ANDa4_ORb, aORb, mergeSubstitutions, u1, u2, t

-}

import AssocList as Dict exposing (Dict)
import Data.Comparison as Comparison
import Data.Dict.Extra as Dict
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


map : (Symbol -> Type) -> Type -> Type
map expandLeaf type_ =
    case type_ of
        Variable symbol ->
            expandLeaf symbol

        Or typeLeft typeRight ->
            Or (map expandLeaf typeLeft) (map expandLeaf typeRight)

        And typeLeft typeRight ->
            And (map expandLeaf typeLeft) (map expandLeaf typeRight)


realizeSubstitutions : Substitutions -> (Symbol -> Type)
realizeSubstitutions substitutions =
    \symbol ->
        Dict.get symbol substitutions
            |> Maybe.withDefault (Variable symbol)


substitute : Substitutions -> (Type -> Type)
substitute =
    realizeSubstitutions >> map


represent : Substitutions -> Type -> Type
represent substitutions originalType =
    substitute substitutions originalType
        |> (\newType ->
                if newType == originalType then
                    newType

                else
                    substitute substitutions newType
           )


variable : String -> Type
variable =
    Symbol.fromString >> Variable


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
mergeSubstitutions =
    Dict.mergeWithPossibleFailOnConflict
        (\symbol type1 type2 accumulatedSubstitutions ->
            unify type1 type2
                |> Maybe.andThen
                    (\type1Withtype2Substitutions ->
                        mergeSubstitutions
                            accumulatedSubstitutions
                            (type1Withtype2Substitutions
                                |> Dict.insert symbol type1
                            )
                    )
        )



-- Testing


aORb : Type
aORb =
    or (variable "a") (variable "b")


a1ANDa2_ORb2 : Type
a1ANDa2_ORb2 =
    or
        (and
            (variable "a1")
            (variable "a2")
        )
        (variable "b2")


a3ANDa4_ORb : Type
a3ANDa4_ORb =
    or
        (and
            (variable "a3")
            (variable "a4")
        )
        (variable "b2")


u1 : Maybe Substitutions
u1 =
    unify aORb a1ANDa2_ORb2


u2 : Maybe Substitutions
u2 =
    unify aORb a3ANDa4_ORb


t : Maybe Type
t =
    Maybe.andThen2 mergeSubstitutions u1 u2
        |> Maybe.map represent
        |> Maybe.andMap (Just aORb)
