module Type.Inference exposing (..)

import Context.Quotient as Context
import Data.Stateful as Stateful exposing (Stateful)
import Data.Stateful.Fallible as StatefulFallible exposing (StatefulFallible)
import Data.Stateful.Fallible2 as StatefulFallible2
import Data.Symbol as Symbol
import Maybe.Extra as Maybe
import Result.Extra as Result
import Type exposing (Type)
import Type.Substitutions exposing (Substitutions)
import Value exposing (Value)



-- Conditional


type alias Conditional a =
    Stateful Assumptions a


type alias Assumptions =
    Context.Quotient


variableIntroduction_ : Value.Symbol -> Type.Symbol -> Conditional (Result String Type)
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
--         Value.Tuple value1 value2 ->
--             StatefulFallible.andThen2
--                 (\type1 type2 ->
--                     StatefulFallible.pure (Type.and type1 type2)
--                 )
--                 (infer value1)
--                 (infer value2)
--


type alias Inference a =
    StatefulFallible2.StatefulFallible { assumptions : Assumptions, seed : Seed } Error a


type Error
    = Error


generateTypeSymbol2 : Inference Type.Symbol
generateTypeSymbol2 =
    \({ seed } as state) ->
        let
            ( newSeed, generatedTypeSymbol ) =
                generateTypeSymbol_ seed
        in
        Ok ( { state | seed = newSeed }, generatedTypeSymbol )


generateTypeVar : Inference Type
generateTypeVar =
    generateTypeSymbol2
        |> StatefulFallible2.map Type.var


variableIntroduction2 : Value.Symbol -> Type.Symbol -> Inference Type
variableIntroduction2 valueSymbol typeSymbol context =
    let
        freshTypeVar =
            Type.var typeSymbol
    in
    Maybe.unwrap (Err Error)
        (\newAssumptions ->
            Ok
                ( { context | assumptions = newAssumptions }
                , freshTypeVar
                )
        )
        (Context.add valueSymbol freshTypeVar context.assumptions)


infer2 : Value -> Inference Type
infer2 term =
    case term of
        Value.Variable valueSymbol ->
            -- TODO refactor
            StatefulFallible2.andThen
                (\typeSymbol ->
                    variableIntroduction2 valueSymbol typeSymbol
                )
                generateTypeSymbol2

        -- Product
        Value.Tuple value1 value2 ->
            Type.and
                |> StatefulFallible2.args2
                    (infer2 value1)
                    (infer2 value2)

        -- Sum
        Value.Left value ->
            Type.or
                |> StatefulFallible2.args2
                    (infer2 value)
                    generateTypeVar

        Value.Right value ->
            Type.or
                |> StatefulFallible2.args2
                    generateTypeVar
                    (infer2 value)



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
