module Context exposing (..)

import AssocList as Dict exposing (Dict)
import Data.Dict.Extra as Dict
import Data.Symbol as Symbol
import Maybe.Extra as Maybe
import Type exposing (Type)
import Value


type alias Context =
    Dict Value.Symbol Type


type alias ContextQuotient =
    { context : Context, substitutions : Type.Substitutions }


empty : Context
empty =
    Dict.empty


singleton : { valueSymbol : Value.Symbol, type_ : Type } -> Context
singleton assignment =
    addUnsafelly assignment empty


quotientMerge : ContextQuotient -> ContextQuotient -> Maybe ContextQuotient
quotientMerge contextQuotient1 contextQuotient2 =
    Maybe.andThen2
        (\mergedContextQuotient mergedSubstitutions ->
            Maybe.map
                (\allMergedSubstitutions ->
                    { context = mergedContextQuotient.context
                    , substitutions = allMergedSubstitutions
                    }
                )
                (Type.mergeSubstitutions mergedContextQuotient.substitutions mergedSubstitutions)
        )
        (merge contextQuotient1.context contextQuotient2.context)
        (Type.mergeSubstitutions contextQuotient1.substitutions contextQuotient2.substitutions)


merge : Context -> Context -> Maybe ContextQuotient
merge =
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
                            Type.mergeSubstitutions type1Withtype2Substitutions contextQuotient.substitutions
                                |> Maybe.map
                                    (\mergedSubstitutions ->
                                        { context = contextQuotient.context |> Dict.insert symbol type1
                                        , substitutions = mergedSubstitutions
                                        }
                                    )
                        )
        }


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
