module Data.String exposing (embrace)


embrace : String -> String
embrace string =
    "{ " ++ string ++ " }"
