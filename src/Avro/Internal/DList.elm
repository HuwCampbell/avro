module Avro.Internal.DList exposing (DList, append, empty, singleton, toList)


type alias DList a =
    List a -> List a


append : DList a -> DList a -> DList a
append =
    (<<)


singleton : a -> DList a
singleton x =
    \xs -> x :: xs


empty : DList a
empty =
    identity


toList : DList a -> List a
toList d =
    d []
