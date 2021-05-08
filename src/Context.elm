module Context exposing (..)

import AssocList as Dict exposing (Dict)
import Data.Dict.Extra as Dict
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


addUnsafelly : { valueSymbol : Value.Symbol, type_ : Type } -> Context -> Context
addUnsafelly { valueSymbol, type_ } context =
    -- rewrites the type if valueSymbol is already assigned to in the context
    Dict.insert valueSymbol type_ context


add : { valueSymbol : Value.Symbol, type_ : Type } -> Context -> Maybe Context
add assignment context =
    merge
        (singleton assignment)
        context


merge : Context -> Context -> Maybe Context
merge =
    -- unsafely (doesn't check for cycles)
    {- non-associative i.e.

         (C1 + C2) + C3 /= C1 + (C2 + C3)

       that is because we are not keeping the Substitutions that result from Type.unify

       Concrete example :
          a)
               ({ a : A1 } + { a : A2 }) + { a : A1 or B }
            ~> ({ a : A2 }             ) + { a : A1 or B }
            ~> { a : A1 or b }
          b)
               { a : A1 } + ({ a : A2 } + { a : A1 or B })
            ~> { a : A1 } + ({ a : A1 or B }             )
            ~> CYCLES (because of Type.findQuotientRepresentative)
    -}
    Dict.mergeWithPossibleFailOnConflict
        (\symbol type1 type2 accCtx ->
            Maybe.map
                (\type1Withtype2Subs ->
                    accCtx
                        |> Dict.insert symbol type1
                        |> Dict.map
                            (\_ type_ ->
                                type_
                                    |> Type.represent type1Withtype2Subs
                            )
                )
                (Type.unify type1 type2)
        )



-- Testing


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
