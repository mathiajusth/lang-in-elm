module Data.Dict.Extra exposing (binaryFallibleFold, binaryFold)

import AssocList as Dict exposing (Dict)


binaryFold :
    { initial : result
    , onOneKeyMatch : key -> value -> result -> result
    , onBothKeyMatch : key -> value -> value -> result -> result
    }
    -> Dict key value
    -> Dict key value
    -> result
binaryFold { initial, onBothKeyMatch, onOneKeyMatch } left right =
    Dict.merge
        onOneKeyMatch
        onBothKeyMatch
        onOneKeyMatch
        left
        right
        initial


binaryFallibleFold :
    { initial : result
    , onBothKeyMatch : key -> value -> value -> result -> Maybe result
    , onOneKeyMatch : key -> value -> result -> Maybe result
    }
    -> Dict key value
    -> Dict key value
    -> Maybe result
binaryFallibleFold { initial, onBothKeyMatch, onOneKeyMatch } left right =
    Dict.merge
        (\key value ->
            Maybe.andThen (onOneKeyMatch key value)
        )
        (\key value1 value2 ->
            Maybe.andThen (onBothKeyMatch key value1 value2)
        )
        (\key value ->
            Maybe.andThen (onOneKeyMatch key value)
        )
        left
        right
        (Just initial)
