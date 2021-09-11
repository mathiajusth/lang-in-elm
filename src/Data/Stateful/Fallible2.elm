module Data.Stateful.Fallible exposing (..)

import Data.Stateful as Stateful exposing (Stateful)
import Result.Extra as Result


type alias StatefulFallible state e a =
    state -> Result e ( state, a )



-- Functor


pure : a -> StatefulFallible state e a
pure a state =
    Ok ( state, a )


pureErr : e -> StatefulFallible state e a
pureErr e state =
    Err e


map : (a -> b) -> StatefulFallible state e a -> StatefulFallible state e b
map aToB statefulA =
    statefulA
        >> Result.map (Tuple.mapSecond aToB)



-- Result.map
-- (\( newState, a ) ->
--     ( newState, aToB a )
-- )
-- (statefulA state)
-- Applicative


apply : StatefulFallible e state (a -> b) -> StatefulFallible e state a -> StatefulFallible e state b
apply statefulAtoB statefulA =
    -- which state update to do first?
    -- how to define this based on monad?
    \state ->
        let
            ( newState, resultAToB ) =
                statefulAtoB state

            ( newestState, resultA ) =
                statefulA newState
        in
        ( newestState
        , Result.andMap resultA resultAToB
        )



-- Monad


applyInKleisli : (a -> StatefulFallible e state b) -> StatefulFallible e state a -> StatefulFallible e state b
applyInKleisli kleisliFunction statefulA state =
    let
        ( newState, resultA ) =
            statefulA state
    in
    Result.unpack (\err -> ( newState, Err err ))
        (\a ->
            kleisliFunction a newState
        )
        resultA


andThen : (a -> StatefulFallible e state b) -> StatefulFallible e state a -> StatefulFallible e state b
andThen =
    applyInKleisli


andThen2 : (a -> b -> StatefulFallible e state c) -> StatefulFallible e state a -> StatefulFallible e state b -> StatefulFallible e state c
andThen2 f statefulA statefulB state =
    let
        ( newState, resultA ) =
            statefulA state

        ( newerState, resultB ) =
            statefulB newState
    in
    Result.map2
        (\a b ->
            f a b newerState
        )
        resultA
        resultB
        |> Result.unpack (\err -> ( newState, Err err ))
            identity
