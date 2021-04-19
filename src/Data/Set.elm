module Data.Set exposing
    ( Nonempty
    , Set
    , andThen
    , concat
    , difference
    , empty
    , foldMap
    , fromList
    , insert
    , intersection
    , isEmpty
    , map
    , member
    , remove
    , singleton
    , size
    , symmetricDifference
    , toList
    , toggle
    , union
    )

import Basics.Extra as Basics
import Data.Set.Internal as InternalSet exposing (NonemptyList)
import Data.Set.Nonempty as NonemptySet
import List.Extra as List
import Maybe.Extra as Maybe



{-
   Libraries that are out have some problems, they

       - allow only sets of comparables to be fast

         (https://package.elm-lang.org/packages/elm/core/latest/Set)

       - store the sorting function in state
         which results in state having one more type parameter
         storing the type that is used for comparison e.g. Int

            (Set Int a)

         which makes empty sets non-isomporhpic
         and you have to provide a comparison function

            Set.empty : (a -> comparable) -> Set comparable a

         This is really inconvenient

         (https://package.elm-lang.org/packages/turboMaCk/any-set/latest/Set-Any)

       - allow for Set.toList with asspulled order

         (https://package.elm-lang.org/packages/erlandsona/assoc-set/latest/AssocSet#toList)

    IMO best is to by default use:
        - Set that doesn't require comparables and is slow + doesn't act like ordered sometimes (like the above-mentioned toList function)
        - Set.Ordered that doesn't require comparables but doesn't store the toComparable function in state

    And if speed is actually of concern (which is rarelly the case for us)
        - Elm.Core.Set for comparables
        - Set.Any for non-comparables
-}


type alias Set a =
    InternalSet.Set a


type alias Nonempty a =
    InternalSet.NonemptySet a



-- Constructors


empty : Set a
empty =
    InternalSet.empty


singleton : a -> Set a
singleton =
    InternalSet.singleton


fromNonempty : Nonempty a -> Set a
fromNonempty =
    InternalSet.nonemptyToSet


fromList : List a -> Set a
fromList =
    InternalSet.fromList


fromNonemptyList : NonemptyList a -> Set a
fromNonemptyList =
    InternalSet.fromNonemptyList



-- Descutrcors


unpack : b -> (Nonempty a -> b) -> Set a -> b
unpack =
    InternalSet.unpack


member : a -> Set a -> Bool
member a =
    InternalSet.unpack False
        (NonemptySet.member a)


isEmpty : Set a -> Bool
isEmpty =
    unpack True (always False)


toList : (a -> comparable) -> Set a -> List a
toList toComparable =
    unpack []
        (NonemptySet.toList toComparable)


toNonempty : Set a -> Maybe (Nonempty a)
toNonempty =
    unpack Nothing Just


size : Set a -> Int
size =
    unpack 0 NonemptySet.size



-- Member control


insert : a -> Set a -> Set a
insert insertee =
    InternalSet.unpack
        (InternalSet.singleton insertee)
        (NonemptySet.insert insertee >> fromNonempty)


remove : a -> Set a -> Set a
remove removee =
    InternalSet.unpack empty (NonemptySet.removeToSet removee)


toggle : a -> Set a -> Set a
toggle togglee set =
    if member togglee set then
        remove togglee set

    else
        insert togglee set



-- Basic functions


union : Set a -> Set a -> Set a
union =
    InternalSet.foldl (always 0) insert


intersection : Set a -> Set a -> Set a
intersection setA setB =
    difference (union setA setB) (symmetricDifference setA setB)


difference : Set a -> Set a -> Set a
difference =
    InternalSet.foldl (always 0) remove


symmetricDifference : Set a -> Set a -> Set a
symmetricDifference =
    InternalSet.foldl (always 0) toggle



-- Functor


map : (a -> b) -> Set a -> Set b
map mapper set =
    InternalSet.unpack empty
        (NonemptySet.map mapper
            >> fromNonempty
        )
        set



-- Applicative


andMap : a -> Set (a -> b) -> Set b
andMap a =
    unpack empty
        (NonemptySet.andMap a
            >> fromNonempty
        )



-- Monad


concat : Set (Set a) -> Set a
concat =
    foldMap identity empty union


kleisliApply : Set a -> (a -> Set b) -> Set b
kleisliApply a f =
    concat <| map f a


andThen : Set a -> (a -> Set b) -> Set b
andThen =
    kleisliApply


mapKleisliToSet : (a -> Set b) -> Set a -> Set b
mapKleisliToSet =
    Basics.flip kleisliApply


kleisliCompose : (a -> Set b) -> (b -> Set c) -> a -> Set c
kleisliCompose f g a =
    f a |> mapKleisliToSet g



-- Quasi Foldable
--   monoid has to be commutative as we don't have order on Set as opposed to e.g. List
--   hence the "Quasi"


foldMap : (a -> m) -> m -> (m -> m -> m) -> Set a -> m
foldMap =
    InternalSet.foldMap


foldl : (actionOnA -> comparable) -> (actionOnA -> a -> a) -> a -> Set actionOnA -> a
foldl =
    InternalSet.foldl
