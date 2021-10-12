module Type.Inference exposing (..)

import Context as Context
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
    = AssumptionError Assumptions.Error



-- lifts


liftSeed : Contextual Seed e a -> Contextual { r | seed : Seed } e a
liftSeed =
    Contextual.embedState .seed (\newSeed ctx -> { ctx | seed = newSeed })


liftAssumptionCtx : Contextual Assumptions e a -> Contextual { r | assumptions : Assumptions } e a
liftAssumptionCtx =
    Contextual.embedState .assumptions (\newAssumptions ctx -> { ctx | assumptions = newAssumptions })


liftAssumptionError : Contextual state Assumptions.Error a -> Contextual state Error a
liftAssumptionError contextualA =
    contextualA
        >> Result.mapError AssumptionError


liftAssumption : Contextual Assumptions Assumptions.Error a -> Contextual { r | assumptions : Assumptions } Error a
liftAssumption =
    liftAssumptionCtx >> liftAssumptionError


addAssumption : Value.Symbol -> Type -> Context -> Result Error Context
addAssumption valueSymbol type_ ctx =
    Assumptions.add valueSymbol type_ ctx.assumptions
        |> Result.mapError AssumptionError
        |> Result.map (\newAssumptions -> { ctx | assumptions = newAssumptions })



-- remove : Value.Symbol -> Quotient -> Quotient
-- remove valueSymbol (Quotient quotient) =
--     Quotient
--         { quotient | context = Context.remove valueSymbol quotient.context }


removeAssumption : Value.Symbol -> Context -> Context
removeAssumption valueSymbol ctx =
    { ctx
        | assumptions = Assumptions.remove valueSymbol ctx.assumptions
    }


addEq : Type -> Type -> Context -> Result Error Context
addEq type1 type2 ctx =
    Assumptions.addEq type1 type2 ctx.assumptions
        |> Result.mapError AssumptionError
        |> Result.map (\newAssumptions -> { ctx | assumptions = newAssumptions })


or =
    Contextual.map2 Type.or


and =
    Contextual.map2 Type.and



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
variableIntroduction valueSymbol typeSymbol =
    let
        fresh : Type
        fresh =
            Type.var typeSymbol
    in
    fresh
        |> Contextual.unpure
            (addAssumption valueSymbol fresh)


infer : Value -> Inference Type
infer term =
    case term of
        Value.Variable valueSymbol ->
            Contextual.andThen
                (\freshA ->
                    freshA
                        |> Contextual.unpure
                            (addAssumption valueSymbol freshA)
                )
                freshTypeVar

        -- Product
        --   intro
        Value.Tuple left right ->
            and (infer left) (infer right)

        --   elim
        Value.ProjLeft hopefullyTuple ->
            Contextual.andThen3
                (\freshA freshB hopefullyAnd ->
                    freshA
                        |> Contextual.unpure
                            (addEq (Type.and freshA freshB) hopefullyAnd)
                )
                freshTypeVar
                freshTypeVar
                (infer hopefullyTuple)

        Value.ProjRight hopefullyTuple ->
            Contextual.andThen3
                (\freshA freshB hopefullyAnd ->
                    freshB
                        |> Contextual.unpure
                            (addEq (Type.and freshA freshB) hopefullyAnd)
                )
                freshTypeVar
                freshTypeVar
                (infer hopefullyTuple)

        -- Sum
        --    intro
        Value.Left value ->
            or (infer value) freshTypeVar

        Value.Right value ->
            or freshTypeVar (infer value)

        Value.Lambda valueSymbol body ->
            Contextual.andThen2
                (\freshA bodyType ->
                    Type.arrow freshA bodyType
                        |> Contextual.unpure
                            (addAssumption valueSymbol freshA
                                >> Result.map (removeAssumption valueSymbol)
                            )
                )
                freshTypeVar
                (infer body)

        Value.Application function arg ->
            (\aToB a freshA freshB ->
                freshB
                    |> Contextual.unpure
                        (addEq (Type.arrow freshA freshB) aToB
                            >> Result.andThen
                                (addEq a freshA)
                        )
            )
                |> Contextual.pure
                |> Contextual.andMap (infer function)
                |> Contextual.andMap (infer arg)
                |> Contextual.andMap freshTypeVar
                |> Contextual.andMap freshTypeVar
                |> Contextual.join


inferNice : Value -> Result Error ( Context.Context, String )
inferNice value =
    infer value emptyContext
        |> Result.map
            (\( ctx, assignedType ) ->
                ( Assumptions.represent ctx.assumptions
                , assignedType
                    |> Assumptions.representType ctx.assumptions
                    |> Type.toString
                )
            )
