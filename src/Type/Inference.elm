module Type.Inference exposing (..)

import Context.Quotient as Assumptions
import Data.Contextual as Contextual exposing (Contextual)
import Data.Stateful as Stateful exposing (Stateful)
import Data.Symbol as Symbol
import Maybe.Extra as Maybe
import Result.Extra as Result
import Type exposing (Type)
import Value exposing (Value)


type alias Inference a =
    Contextual Context Error a


type alias Context =
    { assumptions : Assumptions
    , seed : Seed
    }


emptyContext : Context
emptyContext =
    { assumptions = Assumptions.empty
    , seed = 0
    }


type alias Assumptions =
    Assumptions.Quotient


type Error
    = Error


liftSeed : Contextual Seed e a -> Contextual { r | seed : Seed } e a
liftSeed =
    Contextual.embedState .seed (\newSeed ctx -> { ctx | seed = newSeed })


liftAssumption : Contextual Assumptions e a -> Contextual { r | assumptions : Assumptions } e a
liftAssumption =
    Contextual.embedState .assumptions (\newAssumptions ctx -> { ctx | assumptions = newAssumptions })



-- Generator


type alias Generator a =
    Stateful Seed a


type alias Seed =
    Int


generateTypeSymbol : Generator Type.Symbol
generateTypeSymbol seed =
    ( seed + 1
    , seed
        |> String.fromInt
        |> Symbol.fromString
    )


liftGenerator : Generator a -> Contextual { r | seed : Seed } e a
liftGenerator =
    Contextual.liftStateful .seed (\newSeed ctx -> { ctx | seed = newSeed })



--


freshSymbol : Contextual { r | seed : Seed } e Type.Symbol
freshSymbol =
    generateTypeSymbol
        |> liftGenerator


freshTypeVar : Contextual { r | seed : Seed } e Type
freshTypeVar =
    freshSymbol
        |> Contextual.map Type.var


variableIntroduction : Value.Symbol -> Type.Symbol -> Inference Type
variableIntroduction valueSymbol typeSymbol context =
    let
        fresh : Type
        fresh =
            Type.var typeSymbol
    in
    Maybe.unwrap (Err Error)
        (\newAssumptions ->
            Ok
                ( { context | assumptions = newAssumptions }
                , fresh
                )
        )
        (Assumptions.add valueSymbol fresh context.assumptions)


infer : Value -> Inference Type
infer term =
    case term of
        Value.Variable valueSymbol ->
            Contextual.andThen
                (\freshA ->
                    freshA
                        |> Contextual.unpure
                            (Assumptions.add valueSymbol freshA
                                >> Result.fromMaybe Error
                            )
                        |> liftAssumption
                )
                freshTypeVar

        -- Product
        Value.Tuple value1 value2 ->
            Type.and
                |> Contextual.args2
                    (infer value1)
                    (infer value2)

        -- Sum
        Value.Left value ->
            Type.or
                |> Contextual.args2
                    (infer value)
                    freshTypeVar

        Value.Right value ->
            Type.or
                |> Contextual.args2
                    freshTypeVar
                    (infer value)

        Value.ProjLeft value ->
            Contextual.andThen3
                (\freshA freshB valueType ->
                    freshA
                        |> Contextual.unpure
                            (Assumptions.addEq (Type.and freshA freshB) valueType
                                >> Result.fromMaybe Error
                            )
                        |> liftAssumption
                )
                freshTypeVar
                freshTypeVar
                (infer value)

        Value.ProjRight value ->
            Contextual.andThen3
                (\freshA freshB valueType ->
                    freshB
                        |> Contextual.unpure
                            (Assumptions.addEq (Type.and freshA freshB) valueType
                                >> Result.fromMaybe Error
                            )
                        |> liftAssumption
                )
                freshTypeVar
                freshTypeVar
                (infer value)


inferNice : Value -> Result Error ( Context, String )
inferNice value =
    infer value emptyContext
        |> (Result.map << Tuple.mapSecond) Type.toString
