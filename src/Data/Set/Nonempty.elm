module Data.Set.Nonempty
    exposing
    {- DON'T EXPOSE THESE:
       Set
    -}
    ( NonemptySet
    , andMap
    , foldMap
    , fromElementAndList
    , fromElementAndSet
    , fromList
    , fromSet
    , insert
    , map
    , member
    , removeToSet
    , size
    , toList
    , toSet
    )

import Basics.Extra as Basics
import Data.Set.Internal as InternalSet exposing (NonemptyList)
import List.Nonempty as NonemptyList
import Maybe.Extra as Maybe


type alias Set a =
    InternalSet.Set a


type alias NonemptySet a =
    InternalSet.NonemptySet a



-- Constructors


singleton : a -> NonemptySet a
singleton =
    InternalSet.nonemptySingleton


fromNonemptyList : NonemptyList a -> NonemptySet a
fromNonemptyList =
    InternalSet.nonemptyFromNonemptyList


fromList : List a -> Maybe (NonemptySet a)
fromList =
    NonemptyList.fromList
        >> Maybe.map fromNonemptyList


fromSet : Set a -> Maybe (NonemptySet a)
fromSet =
    InternalSet.nonemptyFromSet


fromElementAndList : a -> List a -> NonemptySet a
fromElementAndList elem list =
    Maybe.withDefault (singleton elem)
        (fromList <| (elem :: list))


fromElementAndSet : a -> Set a -> NonemptySet a
fromElementAndSet elem set =
    InternalSet.unpack
        (singleton elem)
        (insert elem)
        set



-- Descutrcors


toSet : NonemptySet a -> Set a
toSet =
    InternalSet.nonemptyToSet


toNonemptyList : (a -> comparable) -> NonemptySet a -> NonemptyList a
toNonemptyList =
    InternalSet.nonemptyToNonemptyList


toList : (a -> comparable) -> NonemptySet a -> List a
toList toComparable =
    toNonemptyList toComparable >> NonemptyList.toList


member : a -> NonemptySet a -> Bool
member =
    InternalSet.nonemptyMember


uncons : NonemptySet a -> ( a, Set a )
uncons =
    InternalSet.nonemptyUncons


size : NonemptySet a -> Int
size =
    InternalSet.nonemptySize



-- Endomorphisms (type Endomorphism a = a -> a)


insert : a -> NonemptySet a -> NonemptySet a
insert =
    InternalSet.nonemptyInsert


removeOrFail : a -> NonemptySet a -> Maybe (NonemptySet a)
removeOrFail =
    InternalSet.removeOrFail


removeOrIdentity : a -> NonemptySet a -> NonemptySet a
removeOrIdentity =
    InternalSet.removeOrIdentity


removeToSet : a -> NonemptySet a -> Set a
removeToSet =
    InternalSet.remove


toggle : a -> NonemptySet a -> NonemptySet a
toggle togglee set =
    -- doesn't allow to untoggle last element
    if member togglee set then
        removeOrFail togglee set
            |> Maybe.withDefault set

    else
        insert togglee set



-- Functor


map : (a -> b) -> NonemptySet a -> NonemptySet b
map =
    InternalSet.nonemptyMap



-- Applicative


andMap : a -> NonemptySet (a -> b) -> NonemptySet b
andMap =
    InternalSet.nonemptyAndMap



-- Monad


concat : NonemptySet (NonemptySet a) -> NonemptySet a
concat =
    foldMap identity union


kleisliApply : NonemptySet a -> (a -> NonemptySet b) -> NonemptySet b
kleisliApply a f =
    concat <| map f a


andThen : NonemptySet a -> (a -> NonemptySet b) -> NonemptySet b
andThen =
    kleisliApply


mapKleisliToSet : (a -> NonemptySet b) -> NonemptySet a -> NonemptySet b
mapKleisliToSet =
    Basics.flip kleisliApply


kleisliCompose : (a -> NonemptySet b) -> (b -> NonemptySet c) -> a -> NonemptySet c
kleisliCompose f g a =
    f a |> mapKleisliToSet g



-- Quasi Foldable
--   monoid has to be commutative as we don't have order on Set as opposed to e.g. List
--   hence the "Quasi"


foldMap : (a -> m) -> (m -> m -> m) -> NonemptySet a -> m
foldMap =
    InternalSet.nonemptyFoldMap


foldl : (actionOnA -> comparable) -> (actionOnA -> a -> a) -> a -> NonemptySet actionOnA -> a
foldl =
    InternalSet.nonemptyFoldl



-- Basic functions (union, intersection, difference, symmetricDifference) -- will implement on demand


union : NonemptySet a -> NonemptySet a -> NonemptySet a
union =
    foldl (always 0) insert
