module Type exposing (..)

{-| tralala.


# Export

@docs Type, variable, and, or, unify, represent, Substitution, parse, toString


# Testing

@docs a1ANDa2_ORb2, a3ANDa4_ORb, aORb, mergeSubstitutions, u1, u2, t

-}

import AssocList as Dict exposing (Dict)
import Data.Comparison as Comparison
import Data.Dict.Extra as Dict
import Data.Set as Set exposing (Set)
import Data.Symbol as Symbol
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser)
import Type.Internal exposing (..)
import Type.Substitutions



-- Types


type alias Type =
    Type.Internal.Type


type alias Symbol =
    Type.Internal.Symbol



-- Helpers


variableFromString : String -> Type
variableFromString =
    Type.Internal.variableFromString


var =
    Type.Internal.Variable


or =
    Type.Internal.Or


and =
    Type.Internal.And


arrow =
    Type.Internal.Arrow


toSymbolList : Type -> List Symbol
toSymbolList =
    Type.Internal.toSymbolList


expandLeaves : (Symbol -> Type) -> Type -> Type
expandLeaves =
    Type.Internal.expandLeaves


unify =
    Type.Internal.unify



-- Parser


parse : String -> Result (List Parser.DeadEnd) Type
parse =
    Parser.run
        (parser
            |. Parser.end
        )


toString : Type -> String
toString type_ =
    case type_ of
        Variable symbol ->
            Symbol.toString symbol

        Or left right ->
            [ "("
            , toString left
            , "|"
            , toString right
            , ")"
            ]
                |> String.join " "

        And left right ->
            [ "("
            , toString left
            , ","
            , toString right
            , ")"
            ]
                |> String.join " "

        Arrow left right ->
            [ "("
            , toString left
            , "->"
            , toString right
            , ")"
            ]
                |> String.join " "


parser : Parser Type
parser =
    let
        leaf : Parser Type
        leaf =
            Parser.variable
                { start = Char.isAlphaNum
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved =
                    [--   "let"
                     -- , "in"
                     -- , "case"
                     -- , "of"
                    ]
                        |> Set.fromList
                        |> Set.toElmSet
                }
                |> Parser.map Type.Internal.variableFromString

        operator : Parser (Type -> Type -> Type)
        operator =
            Parser.oneOf
                [ Parser.succeed and
                    |. Parser.symbol ","
                , Parser.succeed or
                    |. Parser.symbol "|"
                , Parser.succeed arrow
                    |. Parser.symbol "->"
                ]
    in
    Parser.oneOf
        [ leaf
        , Parser.succeed
            (\leftVar op rightVar ->
                op leftVar rightVar
            )
            |. Parser.symbol "("
            |. Parser.spaces
            |= Parser.lazy (\_ -> parser)
            |. Parser.spaces
            |= operator
            |. Parser.spaces
            |= Parser.lazy (\_ -> parser)
            |. Parser.spaces
            |. Parser.symbol ")"
        ]
