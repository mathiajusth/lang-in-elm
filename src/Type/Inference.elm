module Type.Inference exposing (..)

import Context exposing (Context)
import Type exposing (Type)
import Type.Substitutions exposing (Substitutions)
import Value exposing (Value)


type alias Context =
    { ctx : Context.Context
    , subs : Substitutions
    , varPrefix : List Int
    }


type alias Inference a =
    Context -> ( Context, a )



-- Inf (Inf a)
--   -> Context -> (Context, Inference a)
--   -> Context -> (Context, Context -> (Context, a))


type Term
    = Leaf TermSymbol
    | Tuple Tuple


type Tuple
    = Introduction Term Term
    | Elimination Elimination


type Elimination
    = ProjL
    | ProjR


type alias TermSymbol =
    String



-- Applicative


apply : Inference (a -> b) -> Inference a -> Inference b
apply infAtoB infA =
    -- which state update to do first?
    -- how to define this based on monad?
    \ctx ->
        let
            ( newCtx, aToB ) =
                infAtoB ctx

            ( newestCtx, a ) =
                infA newCtx
        in
        ( newestCtx, aToB a )



-- Monad


applyInKleisli : (a -> Inference b) -> Inference a -> Inference b
applyInKleisli kleisliFunction kleisliArgument =
    \ctx ->
        let
            ( newCtx, a ) =
                kleisliArgument ctx

            infB =
                kleisliFunction a
        in
        infB newCtx


andThen : Inference a -> (a -> Inference b) -> Inference b
andThen kleisliArgument kleisliFunction =
    applyInKleisli kleisliFunction kleisliArgument



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
