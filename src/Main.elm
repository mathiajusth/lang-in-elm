module Main exposing (..)

import Zero exposing (Zero)


type One
    = One


type SetObject
    = Zero_ Zero
    | One_ One
      -- | Arrow (Arrow SetObject SetObject)
    | Or (List SetObject)
    | And (List SetObject)


one : SetObject
one =
    One_ One


zero : SetObject
zero =
    Zero_ Zero.zero


type ArrowsFromZeroTo codomain
    = EmptyArrow


type ArrowsFromOneTo codomain
    = ElementInCodomain codomain


type ArrowsFromOr_ arrows
    = List arrows


type ArrowsFromOr
    = ArrowsFromOr_


type ArrowTo codomain
    = FromZero (ArrowsFromZeroTo codomain)
    | FromOne (ArrowsFromOneTo codomain)
    | FromOr (List (ArrowTo codomain))
    | FromAnd (List (ArrowTo codomain))



-- arrow examples


identityOnOne : ArrowTo One
identityOnOne =
    FromOne (ElementInCodomain One)


empty : ArrowTo any
empty =
    FromZero EmptyArrow


bool : SetObject
bool =
    Or [ one, one ]


three : SetObject
three =
    Or [ one, one, one ]



-- subsetOfThree : ArrowTo
-- subsetOfThree =
--     FromOr []
-- arrowToObject : Arrow SetObject SetObject -> SetObject
-- arrowToObject setArrow =
--     case setArrow of
--         FromZero ArrowFromZeroTo ->
--             Arrow <| FromZero ArrowFromZeroTo
--         FromOne (ElementInCodomain elementInCodomain) ->
--             case elementInCodomain of
--                 Zero _ ->
--                     Arrow <| FromZero ArrowFromZeroTo
--                 One () ->
--                     Arrow <| FromOne (ElementInCodomain <| One ())
-- oneId2 : SetObject
-- oneId2 =
--     Arrow <| FromOne (ElementInCodomain <| One ())
-- type alias Arrow =
--     Arrow SetObject SetObject


type TypeSymbol
    = Type String


type ValueSymbol
    = Value String



-- type Or
--     = Left TypeSymbol
--     | Right TypeSymbol
-- type And
--     = First TypeSymbol
--     | Second TypeSymbol
-- type AtomicOrAnd s
--     = Atomic s
--     | Or s
--     | And s


set =
    [ Type "Zero"
    , Type "One"
    ]


one =
    ()
