module Avro.Value.Int64 exposing
    ( Int64
    , fromInt, toInt, fromPair, toPair, toFloat
    )

{-| This module implents a signed 64 bit integer.

Javascript platforms do not natively have a signed 64
bit integer type, thought the Avro specification
assumes the existence of one.

@docs Int64

@docs fromInt, toInt, fromPair, toPair, toFloat

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
javascript safe integer range.

-}
fromInt : Int -> Int64
fromInt =
    Int64.fromInt53


{-| Build a standard Int from an Int64.

This will return integers within the javacript safe
range; values outside of this will result in a `Nothing`.

-}
toInt : Int64 -> Maybe Int
toInt =
    Int64.toInt53


{-| Build a long from upper and lower bits of ints.
-}
fromPair : Int -> Int -> Int64
fromPair =
    Int64.fromInts


{-| Render an Int64 to a pair of upper and lower bit ints.
-}
toPair : Int64 -> ( Int, Int )
toPair =
    Int64.toInts


{-| Create a standard `Float` value from an Int64.

This will lose precision beyond the javascript safe integer range.

-}
toFloat : Int64 -> Float
toFloat =
    Int64.toFloat
