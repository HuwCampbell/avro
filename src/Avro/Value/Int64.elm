module Avro.Value.Int64 exposing
    ( Int64
    , fromInt, fromPair, toFloat, toInt, toPair
    )

{-| Long representation

@docs Int64

@docs fromInt, fromPair, toFloat, toInt, toPair

-}

import Avro.Internal.Int64 as Int64


{-| A 64 bit signed integer.

This type is encoded using two Int values by the
concatentation of their bitwise representations.

-}
type alias Int64 =
    Int64.Int64


{-| Create a 64 bit integer from an Elm Int.

This is guaranteed to work for integers within the
javascript safe range.

-}
fromInt : Int -> Int64
fromInt =
    Int64.fromInt53


{-| Coerce a 64 bit integer as an Elm Int.

This will return integers within the javacript safe
range.

-}
toInt : Int64 -> Maybe Int
toInt =
    Int64.toInt53


{-| Build a long from a pair of ints.
-}
fromPair : Int -> Int -> Int64
fromPair =
    Int64.fromInts


{-| Render an Int64 to a pair of ints.
-}
toPair : Int64 -> ( Int, Int )
toPair =
    Int64.toInts


{-| Coerce a 64 bit integer as an Elm Float.

This will lose precision beyond the Elm safe
range.

-}
toFloat : Int64 -> Float
toFloat =
    Int64.toFloat
