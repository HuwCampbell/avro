module Avro.Internal.DList exposing (..)


type alias DList a =
    List a -> List a


append : DList a -> DList a -> DList a
append =
    (<<)


cons : a -> DList a -> DList a
cons x xs =
    append (singleton x) xs


snoc : DList a -> a -> DList a
snoc xs x =
    append xs (singleton x)


singleton : a -> DList a
singleton x =
    \xs -> x :: xs


empty : DList a
empty =
    identity
