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



-- mapErr : (e -> f) -> Stateful state e a -> Stateful state f a
-- mapErr eToF statefulA state =
--     let
--         ( newState, a ) =
--             statefulA state
--     in
--     ( newState, aToB a )
