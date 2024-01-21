module Avro.Internal.DList exposing (..)


type alias DList a =
    List a -> List a


append : DList a -> DList a -> DList a
append =
    (<<)


snoc : DList a -> a -> DList a
snoc xs x =
    append xs (singleton x)


singleton : a -> DList a
singleton x =
    \xs -> x :: xs


empty : DList a
empty =
    identity


toList : DList a -> List a
toList d =
    d []
