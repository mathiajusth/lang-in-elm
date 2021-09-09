module Data.Stateful.Fallible exposing (..)

import Data.Stateful as Stateful exposing (Stateful)
import Result.Extra as Result


type alias StatefulFallible state e a =
    Stateful state (Result e a)



-- Functor


pure : a -> StatefulFallible state e a
pure a state =
    ( state, Ok a )


pureErr : e -> StatefulFallible state e a
pureErr e state =
    ( state, Err e )


map : (a -> b) -> StatefulFallible state e a -> StatefulFallible state e b
map aToB statefulA state =
    let
        ( newState, resultA ) =
            statefulA state
    in
    Result.unpack (\err -> ( newState, Err err ))
        (\a ->
            ( newState, Ok <| aToB a )
        )
        resultA



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
