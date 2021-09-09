module Type.Inference exposing (..)

import Context.Quotient as Context
import Data.Stateful as Stateful exposing (Stateful)
import Data.Symbol as Symbol
import Maybe.Extra as Maybe
import Type exposing (Type)
import Type.Substitutions exposing (Substitutions)
import Value exposing (Value)



-- Conditional


type alias Conditional a =
    Stateful Assumptions a


type alias Assumptions =
    Context.Quotient


variableIntroduction_ : Value.Symbol -> Type.Symbol -> Maybe (Conditional Type)
variableIntroduction_ valueSymbol typeSymbol assumptions =
    let
        freshTypeVar =
            Type.var typeSymbol
    in
    Maybe.unwrap
        ( assumptions, Err "Assumption merge fail" )
        (\newAssumptions ->
            ( newAssumptions
            , Ok freshTypeVar
            )
        )
        (Context.add valueSymbol freshTypeVar assumptions)



-- Generator


type alias Generator a =
    Stateful Seed a


type alias Seed =
    -- TODO just mock
    Type.Symbol


generateTypeSymbol_ : Generator Type.Symbol
generateTypeSymbol_ seed =
    ( seed
        |> Symbol.toString
        |> (++) "_"
        |> Symbol.fromString
    , seed
    )



-- Contextual


type alias Contextual a =
    Stateful ( Assumptions, Seed ) a


liftConditional : Conditional a -> Contextual a
liftConditional =
    Stateful.liftLeft


liftGenerator : Generator a -> Contextual a
liftGenerator =
    Stateful.liftRight


generateTypeSymbol : Contextual Type.Symbol
generateTypeSymbol =
    liftGenerator generateTypeSymbol_


variableIntroduction : Value.Symbol -> Type.Symbol -> Contextual (Result String Type)
variableIntroduction valueSymbol typeSymbol =
    liftConditional (variableIntroduction_ valueSymbol typeSymbol)



-- infer : Value -> Contextual (Result String Type)
-- infer term =
--     case term of
--         Value.Variable valueSymbol ->
--             Stateful.andThen
--                 (\typeSymbol ->
--                     variableIntroduction valueSymbol typeSymbol
--                 )
--                 generateTypeSymbol
--         -- Product
--         Tuple value1 value2 ->
--           Stateful.pure
--               (\resultType1 resultType2 ->
--                 Result.map2
--                   (\type1 type2 ->
--                   )
--                   resultType1
--                   resultType2
--               )
--               |> State.andMap (infer v1)
--               |> State.andMap (infer v2)
-- andMap : Inference a -> Inference (a -> b) -> Inference b
-- andMap infA infAtoB =
--   \ctx ->
-- freshVar : Inference TermSymbol
-- freshVar =
--   \ctx ->
--     (
--       )
-- infer : Term -> Inference Type
-- infer term =
--   case term of
--     Leaf termSymbol ->
--       \ctx ->
--         ({ctx | })
-- emptyState : State
-- emptyState =
--     { ctx = Context.empty
--     , subs = Type.Substitutions.empty
--     , varsCount = 0
--     }
-- pure : a ->  Inference a
-- pure =
