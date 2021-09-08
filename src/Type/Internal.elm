module Type.Internal exposing (..)

import AssocList as Dict exposing (Dict)
import Data.Comparison as Comparison
import Data.Dict.Extra as Dict
import Data.Symbol as Symbol
import Maybe.Extra as Maybe


type Type
    = Variable Symbol
    | Or Type Type
    | And Type Type
    | Arrow Type Type


type alias Substitutions =
    Dict Symbol Type


type alias Symbol =
    Symbol.Symbol TypeSymbolTag


type TypeSymbolTag
    = TypeSymbolTag


variableFromString : String -> Type
variableFromString =
    Symbol.fromString >> Variable


toSymbolList : Type -> List Symbol
toSymbolList type_ =
    case type_ of
        Variable symbol ->
            [ symbol ]

        Or leftType rightType ->
            toSymbolList leftType ++ toSymbolList rightType

        And leftType rightType ->
            toSymbolList leftType ++ toSymbolList rightType

        Arrow leftType rightType ->
            toSymbolList leftType ++ toSymbolList rightType


expandLeaves : (Symbol -> Type) -> Type -> Type
expandLeaves expandLeaf type_ =
    case type_ of
        Variable symbol ->
            expandLeaf symbol

        Or typeLeft typeRight ->
            Or (expandLeaves expandLeaf typeLeft) (expandLeaves expandLeaf typeRight)

        And typeLeft typeRight ->
            And (expandLeaves expandLeaf typeLeft) (expandLeaves expandLeaf typeRight)

        Arrow typeLeft typeRight ->
            Arrow (expandLeaves expandLeaf typeLeft) (expandLeaves expandLeaf typeRight)


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

        ( And _ _, _ ) ->
            Nothing

        ( _, And _ _ ) ->
            Nothing

        ( Arrow type1Left type1Right, Arrow type2Left type2Right ) ->
            Maybe.map2 mergeSubstitutions (unify type1Left type2Left) (unify type1Right type2Right)
                |> Maybe.join


mergeSubstitutions : Substitutions -> Substitutions -> Maybe Substitutions
mergeSubstitutions =
    Dict.binaryFallibleFold
        { initial = Dict.empty
        , onOneKeyMatch =
            \symbol type_ substitutions ->
                Just <| Dict.insert symbol type_ substitutions
        , onBothKeyMatch =
            \symbol type1 type2 substitutions ->
                unify type1 type2
                    |> Maybe.andThen
                        (\type1Withtype2Substitutions ->
                            mergeSubstitutions
                                substitutions
                                (type1Withtype2Substitutions
                                    |> Dict.insert symbol type1
                                )
                        )
        }
