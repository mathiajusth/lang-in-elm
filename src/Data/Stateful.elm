module Data.Stateful exposing (..)


type alias Stateful state a =
    state -> ( state, a )



-- Functor


pure : a -> Stateful state a
pure a state =
    ( state, a )


map : (a -> b) -> Stateful state a -> Stateful state b
map aToB statefulA state =
    let
        ( newState, a ) =
            statefulA state
    in
    ( newState, aToB a )



-- Applicative


apply : Stateful state (a -> b) -> Stateful state a -> Stateful state b
apply statefulAtoB statefulA =
    -- which state update to do first?
    -- how to define this based on monad?
    \state ->
        let
            ( newState, aToB ) =
                statefulAtoB state

            ( newestState, a ) =
                statefulA newState
        in
        ( newestState, aToB a )



-- Monad


applyInKleisli : (a -> Stateful state b) -> Stateful state a -> Stateful state b
applyInKleisli kleisliFunction kleisliArgument =
    \ctx ->
        let
            ( newCtx, a ) =
                kleisliArgument ctx

            infB =
                kleisliFunction a
        in
        infB newCtx


andThen : (a -> Stateful state b) -> Stateful state a -> Stateful state b
andThen =
    applyInKleisli



-- state tuple


type alias Product state1 state2 a =
    Stateful ( state1, state2 ) a


liftLeft : Stateful state1 a -> Product state1 state2 a
liftLeft stateful1A ( state1, state2 ) =
    let
        ( newState1, a ) =
            stateful1A state1
    in
    ( ( newState1, state2 ), a )


liftRight : Stateful state2 a -> Product state1 state2 a
liftRight stateful1A ( state1, state2 ) =
    let
        ( newState2, a ) =
            stateful1A state2
    in
    ( ( state1, newState2 ), a )
