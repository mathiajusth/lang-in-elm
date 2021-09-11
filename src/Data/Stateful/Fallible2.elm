module Data.Stateful.Fallible2 exposing (..)

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


apply : StatefulFallible e state (a -> b) -> StatefulFallible e state a -> StatefulFallible e state b
apply statefulAtoB statefulA =
    -- which state update to do first?
    -- how to define this based on monad?
    statefulAtoB
        >> Result.andThen
            (\( newState, aTob ) ->
                statefulA newState
                    |> Result.map
                        (\( newerState, a ) ->
                            ( newerState, aTob a )
                        )
            )


applyInKleisli : (a -> StatefulFallible e state b) -> StatefulFallible e state a -> StatefulFallible e state b
applyInKleisli kleisliFunction statefulA =
    statefulA
        >> Result.andThen
            (\( newState, a ) ->
                kleisliFunction a newState
            )


andThen : (a -> StatefulFallible e state b) -> StatefulFallible e state a -> StatefulFallible e state b
andThen =
    applyInKleisli


andThen2 : (a -> b -> StatefulFallible e state c) -> StatefulFallible e state a -> StatefulFallible e state b -> StatefulFallible e state c
andThen2 f statefulA statefulB =
    statefulA
        >> Result.andThen
            (\( newState, a ) ->
                statefulB newState
                    |> Result.andThen
                        (\( newerState, b ) ->
                            f a b newerState
                        )
            )
