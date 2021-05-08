module Context exposing (..)

import AssocList as Dict exposing (Dict)
import Data.Symbol as Symbol
import Type exposing (Type)
import Value


type alias Context =
    Dict Value.Symbol Type


empty : Context
empty =
    Dict.empty


singleton : { valueSymbol : Value.Symbol, type_ : Type } -> Context
singleton assignment =
    addUnsafelly assignment empty


merge : Context -> Context -> Maybe ( Context, Type.Substitutions )
merge context1 context2 =
    -- unsafely (doesn't check for cycles)
    Dict.merge
        (\symbol type_ ->
            Maybe.map
                (\( accCtx, accSubs ) ->
                    ( Dict.insert symbol type_ accCtx, accSubs )
                )
        )
        (\symbol type1 type2 ->
            Maybe.andThen
                (\( accCtx, accSubs ) ->
                    Type.unify type1 type2
                        |> Maybe.andThen
                            (\type1Withtype2Substitutions ->
                                Type.mergeSubstitutions type1Withtype2Substitutions accSubs
                                    |> Maybe.map
                                        (\mergedSubstitutions ->
                                            ( accCtx |> Dict.insert symbol type1
                                            , mergedSubstitutions
                                            )
                                        )
                            )
                )
        )
        (\symbol type_ ->
            Maybe.map
                (\( accCtx, accSubs ) ->
                    ( Dict.insert symbol type_ accCtx, accSubs )
                )
        )
        context1
        context2
        (Just ( Dict.empty, Dict.empty ))



-- Testing


addUnsafelly : { valueSymbol : Value.Symbol, type_ : Type } -> Context -> Context
addUnsafelly { valueSymbol, type_ } context =
    -- rewrites the type if valueSymbol is already assigned to in the context
    Dict.insert valueSymbol type_ context


addTrivial : String -> String -> Context -> Context
addTrivial =
    \valueSymbolName typeName ->
        addUnsafelly
            { valueSymbol = Symbol.fromString valueSymbolName
            , type_ = Type.variable typeName
            }


addNontrivial : String -> Type -> Context -> Context
addNontrivial =
    \valueSymbolName type_ ->
        addUnsafelly
            { valueSymbol = Symbol.fromString valueSymbolName
            , type_ = type_
            }


orTrivial : String -> String -> Type
orTrivial string1 string2 =
    Type.or
        (Type.variable string1)
        (Type.variable string2)


ctx1 : Context
ctx1 =
    empty
        |> addTrivial "a" "A"


ctx2 : Context
ctx2 =
    empty
        |> addTrivial "b" "B"


ctx3 : Context
ctx3 =
    empty
        |> addNontrivial "a" (orTrivial "A" "B")
