module Data.Set.Internal
    {- Internal means we can't import this anywhere except Set modules -}
    exposing
    {- NEVER EXPOSE THESE
       , NonemptySet(..) -- would bypass deduplication of underyling list
       , applyTyUnderlyingList
       , mapUnderlyingList
    -}
    ( NonemptyList
    , NonemptySet
    , Set
    , empty
    , foldMap
    , foldl
    , fromList
    , fromNonemptyList
    , nonemptyAndMap
    , nonemptyFoldMap
    , nonemptyFoldl
    , nonemptyFromNonemptyList
    , nonemptyFromSet
    , nonemptyInsert
    , nonemptyMap
    , nonemptyMember
    , nonemptySingleton
    , nonemptySize
    , nonemptyToNonemptyList
    , nonemptyToSet
    , nonemptyUncons
    , remove
    , removeOrFail
    , removeOrIdentity
    , singleton
    , toElmSet
    , unpack
    )

{- This module exist to

   - define Set and NonemptySet at the same place
     so that both Set and Set.Nonempty can import both
     and define from and to transformation between them
     (otherwise there would be cyclic dependendcy)

     Note: this forces us to use empty and nonEmptySetToSet
     aliases of constructors because Empty and Nonempty
     won't be accessible to users by importing Set
     as there the Set is just an alias of Set.Internal.Set

   - provide safe constructors
     to ensure that Set doesn't have duplicate members
     (e.g. nonemptyFromNonemptyList, nonEmptySetToSet)
     To ensure this safety
     we can never expose NonemptySet constructor

-}

import List.Nonempty as NonemptyList
import Maybe.Extra as Maybe
import Set as ElmSet



---- NONEMPTY SET


type NonemptySet a
    = NonemptySet (NonemptyList a)



-- Constructors (TO)


nonemptySingleton : a -> NonemptySet a
nonemptySingleton =
    NonemptySet << NonemptyList.fromElement


nonemptyFromNonemptyList : NonemptyList a -> NonemptySet a
nonemptyFromNonemptyList =
    NonemptySet << NonemptyList.uniq


nonemptyFromSet : Set a -> Maybe (NonemptySet a)
nonemptyFromSet =
    unpack Nothing Just



-- Descutrcors (FROM)


nonemptyUncons : NonemptySet a -> ( a, Set a )
nonemptyUncons =
    applyTyUnderlyingList
        (\nonemptyList ->
            case nonemptyList of
                NonemptyList.Nonempty head tailList ->
                    ( head, fromList tailList )
        )


nonemptyMember : a -> NonemptySet a -> Bool
nonemptyMember a =
    applyTyUnderlyingList (NonemptyList.member a)


nonemptyToNonemptyList : (a -> comparable) -> NonemptySet a -> NonemptyList a
nonemptyToNonemptyList toComparable (NonemptySet innerNonemptyList) =
    NonemptyList.sortBy toComparable innerNonemptyList


nonemptySize : NonemptySet a -> Int
nonemptySize =
    applyTyUnderlyingList NonemptyList.length



-- Endomorphism (FROM and also TO)


nonemptyInsert : a -> NonemptySet a -> NonemptySet a
nonemptyInsert insertee =
    mapUnderlyingList
        (\innerNonemptyList ->
            if NonemptyList.member insertee innerNonemptyList then
                innerNonemptyList

            else
                NonemptyList.cons insertee innerNonemptyList
        )


removeOrFail : a -> NonemptySet a -> Maybe (NonemptySet a)
removeOrFail removee =
    applyTyUnderlyingList
        (NonemptyList.toList
            >> List.filter ((/=) removee)
            >> NonemptyList.fromList
            >> Maybe.map nonemptyFromNonemptyList
        )


removeOrIdentity : a -> NonemptySet a -> NonemptySet a
removeOrIdentity removee nonemptySet =
    applyTyUnderlyingList
        (NonemptyList.toList
            >> List.filter ((/=) removee)
            >> NonemptyList.fromList
            >> Maybe.unwrap nonemptySet nonemptyFromNonemptyList
        )
        nonemptySet


remove : a -> NonemptySet a -> Set a
remove removee =
    applyTyUnderlyingList
        (NonemptyList.toList
            >> List.filter ((/=) removee)
            >> NonemptyList.fromList
            >> Maybe.unwrap empty
                fromNonemptyList
        )



-- Functor


nonemptyMap : (a -> b) -> NonemptySet a -> NonemptySet b
nonemptyMap mapper =
    mapUnderlyingList (NonemptyList.map mapper)



-- Applicative


nonemptyAndMap : a -> NonemptySet (a -> b) -> NonemptySet b
nonemptyAndMap a =
    mapUnderlyingList (NonemptyList.map ((|>) a))



-- Quasi Foldable
--   monoid has to be commutative as we don't have order on Set as opposed to e.g. List
--   hence the "Quasi"


nonemptyFoldMap : (a -> m) -> (m -> m -> m) -> NonemptySet a -> m
nonemptyFoldMap toComutativeMonoid monoidMultiplication =
    nonemptyUncons
        >> (\( head, set ) ->
                foldMap toComutativeMonoid (toComutativeMonoid head) monoidMultiplication set
           )


nonemptyFoldl : (actionOnA -> comparable) -> (actionOnA -> a -> a) -> a -> NonemptySet actionOnA -> a
nonemptyFoldl toComparable applyAction initialA =
    nonemptyToNonemptyList toComparable
        >> NonemptyList.foldl applyAction initialA



-- Helpers


mapUnderlyingList : (NonemptyList a -> NonemptyList b) -> NonemptySet a -> NonemptySet b
mapUnderlyingList f =
    nonemptyToNonemptyList (always 0) >> f >> nonemptyFromNonemptyList


applyTyUnderlyingList : (NonemptyList a -> b) -> NonemptySet a -> b
applyTyUnderlyingList f =
    nonemptyToNonemptyList (always 0) >> f



---- SET


type Set a
    = Empty
    | Nonempty (NonemptySet a)



-- Set Constructors


empty : Set a
empty =
    Empty


singleton : a -> Set a
singleton a =
    Nonempty <| NonemptySet <| NonemptyList.fromElement a


fromNonemptyList : NonemptyList a -> Set a
fromNonemptyList =
    Nonempty << nonemptyFromNonemptyList


fromList : List a -> Set a
fromList =
    NonemptyList.fromList
        >> Maybe.unwrap empty
            fromNonemptyList


nonemptyToSet : NonemptySet a -> Set a
nonemptyToSet =
    Nonempty



-- Descutrcors


unpack : b -> (NonemptySet a -> b) -> Set a -> b
unpack defaultForEmpty fromNonempty set =
    case set of
        Empty ->
            defaultForEmpty

        Nonempty nonempty ->
            fromNonempty nonempty


toElmSet : Set comparable -> ElmSet.Set comparable
toElmSet =
    unpack
        ElmSet.empty
        (\(NonemptySet nonemptyList) ->
            NonemptyList.toList nonemptyList
                |> ElmSet.fromList
        )



-- Quasi Foldable
--   monoid has to be commutative as we don't have order on Set as opposed to e.g. List
--   hence the "Quasi"


foldMap : (a -> m) -> m -> (m -> m -> m) -> Set a -> m
foldMap toComutativeMonoid monoidNeutralElement monoidMultiplication =
    unpack monoidNeutralElement
        (nonemptyToNonemptyList (always 0)
            >> NonemptyList.map toComutativeMonoid
            >> NonemptyList.foldl monoidMultiplication monoidNeutralElement
        )


foldl : (actionOnA -> comparable) -> (actionOnA -> a -> a) -> a -> Set actionOnA -> a
foldl toComparable applyAction a =
    unpack a (nonemptyFoldl toComparable applyAction a)



--- just convenience


type alias NonemptyList a =
    NonemptyList.Nonempty a
