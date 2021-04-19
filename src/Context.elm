module Context exposing (..)

import Dict exposing (Dict)
import Type exposing (Type)
import Value


type alias Context =
    Dict Value.Symbol Type


add : ValueVar -> Type -> Context -> Context
add valueVar type_ context =
    Debug.todo "TODO add"



-- unify : Context -> Context -> Context
-- unify ctx1 ctx2 =
--     -- unsafely (doesn't check for cycles)
--     Dict.merge
--         Dict.insert
--         (\valueVar type1 type2 accCtx ->
--             mkTypeSubstitutions type1 type2
--                 |> applyTypeSubstitutions {- this can cycle -} accCtx
--         )
--         Dict.insert
--         Dict.empty
--         ctx1
--         ctx2
--
--
-- mkTypeSubstitutions : Type -> Type -> TypeSubstitutions
-- mkTypeSubstitutions type1 type2 =
--     -- case ( type1, type2 ) of
--     --     ( Type.Var string, _ ) ->
--     --         [ ( string, type2 ) ]
--     --     ( _, Type.Var string ) ->
--     --         [ ( string, type1 ) ]
--     --     ( Type.Or type11 type12, Type.Or type21 type22 ) ->
--     --         mkTypeSubstitutions type11 type21
--     --             ++ mkTypeSubstitutions type12 type22
--     --     ( Type.And type11 type12, Type.And type21 type22 ) ->
--     --         mkTypeSubstitutions type11 type21
--     --             ++ mkTypeSubstitutions type12 type22
--     Debug.todo "TODOa mkTypeSubstitutions"
-- applyTypeSubstitutions : TypeSubstitutions -> Context -> Context
-- applyTypeSubstitutions typeSubstitutions context =
--     -- TODO this can cycle
--     Debug.todo "TODO applyTypeSubstitutions"


type alias ValueVar =
    String


type alias TypeVar =
    String
