module Data.Contextual exposing (..)

import Basics.Extra as Basics
import Data.Stateful as Stateful exposing (Stateful)
import Maybe.Extra as Maybe
import Result.Extra as Result


type alias Contextual state e a =
    state -> Result e ( state, a )



-- lifts


liftStatefulTrue : Stateful state a -> Contextual state e a
liftStatefulTrue statefulA state =
    let
        ( newState, a ) =
            statefulA state
    in
    Ok ( newState, a )


liftStateful : (ctx -> state) -> (state -> ctx -> ctx) -> Stateful state a -> Contextual ctx e a
liftStateful getStateFromCtx updateStateInCtx statefulA ctx =
    let
        ( newState, a ) =
            statefulA <| getStateFromCtx ctx
    in
    Ok ( updateStateInCtx newState ctx, a )


unpure : (ctx -> Result e ctx) -> a -> Contextual ctx e a
unpure toResult a ctx =
    toResult ctx
        |> Result.map (\newCtx -> ( newCtx, a ))


embedState : (biggerState -> state) -> (state -> biggerState -> biggerState) -> Contextual state e a -> Contextual biggerState e a
embedState getSmaller updateInBigger stateCtxA biggerState =
    biggerState
        |> getSmaller
        |> stateCtxA
        |> Result.map
            (\( newSmaller, a ) ->
                ( updateInBigger newSmaller biggerState, a )
            )



-- Functor


pure : a -> Contextual state e a
pure a state =
    Ok ( state, a )


pureErr : e -> Contextual state e a
pureErr e state =
    Err e


map : (a -> b) -> Contextual state e a -> Contextual state e b
map aToB statefulA =
    statefulA
        >> Result.map (Tuple.mapSecond aToB)


map2 : (a -> b -> c) -> Contextual state e a -> Contextual state e b -> Contextual state e c
map2 f statefulA statefulB =
    pure f
        |> andMap statefulA
        |> andMap statefulB


apply : Contextual e state (a -> b) -> Contextual e state a -> Contextual e state b
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


andMap : Contextual e state a -> Contextual e state (a -> b) -> Contextual e state b
andMap =
    Basics.flip apply


andMap2 : Contextual e state a -> Contextual e state b -> (a -> b -> c) -> Contextual e state c
andMap2 statefulA statefulB f =
    map2 f statefulA statefulB


applyInKleisli : (a -> Contextual e state b) -> Contextual e state a -> Contextual e state b
applyInKleisli kleisliFunction statefulA =
    statefulA
        >> Result.andThen
            (\( newState, a ) ->
                kleisliFunction a newState
            )


andThen : (a -> Contextual e state b) -> Contextual e state a -> Contextual e state b
andThen =
    applyInKleisli


andThen2 : (a -> b -> Contextual e state c) -> Contextual e state a -> Contextual e state b -> Contextual e state c
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


andThen3 : (a -> b -> c -> Contextual e state d) -> Contextual e state a -> Contextual e state b -> Contextual e state c -> Contextual e state d
andThen3 f statefulA statefulB statefulC =
    statefulA
        >> Result.andThen
            (\( newState, a ) ->
                statefulB newState
                    |> Result.andThen
                        (\( newerState, b ) ->
                            statefulC newerState
                                |> Result.andThen
                                    (\( newestState, c ) ->
                                        f a b c newestState
                                    )
                        )
            )


join : Contextual state e (Contextual state e a) -> Contextual state e a
join =
    andThen identity
