module Data.Dict.Extra exposing (mergeWithPossibleFailOnConflict)

import AssocList as Dict exposing (Dict)


mergeWithPossibleFailOnConflict : (k -> v -> v -> Dict k v -> Maybe (Dict k v)) -> Dict k v -> Dict k v -> Maybe (Dict k v)
mergeWithPossibleFailOnConflict conflictResolver dict1 dict2 =
    Dict.merge
        (\k v ->
            Maybe.map (Dict.insert k v)
        )
        (\k v1 v2 accumulatedMaybeDict ->
            Maybe.andThen
                (\accDict ->
                    conflictResolver k v1 v2 accDict
                )
                accumulatedMaybeDict
        )
        (\symbol type_ ->
            Maybe.map (Dict.insert symbol type_)
        )
        dict1
        dict2
        (Just Dict.empty)
