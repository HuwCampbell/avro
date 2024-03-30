module Avro.Internal.Int64 exposing (Int64, and, fromInt, fromInt53, fromInts, negate, or, popBase128, shiftLeftBy, shiftRightBy63, shiftRightZfBy, toFloat, toInt53, toInts, xor)

import Bitwise


type Int64
    = Int64 Ints


type alias Ints =
    { higher : Int, lower : Int }


fromInt : Int -> Int64
fromInt lower =
    Int64 { higher = 0, lower = Bitwise.or 0 lower }


fromInts : Int -> Int -> Int64
fromInts higher lower =
    Int64 { higher = Bitwise.or 0 higher, lower = Bitwise.or 0 lower }


toInts : Int64 -> ( Int, Int )
toInts (Int64 { higher, lower }) =
    ( higher, lower )


popBase128 : Int64 -> ( Int, Int64 )
popBase128 ((Int64 { lower }) as int) =
    let
        base128 =
            Bitwise.and 0x7F lower

        higherBits =
            shiftRightZfBy 7 int
    in
    ( base128, higherBits )



-- HELPERS


shiftRightZfBy : Int -> Int64 -> Int64
shiftRightZfBy n (Int64 { higher, lower }) =
    if n > 32 then
        fromInts 0 (Bitwise.shiftRightZfBy (32 - n) higher)

    else
        let
            carry =
                Bitwise.shiftLeftBy (32 - n) higher

            newLower =
                lower
                    |> Bitwise.shiftRightZfBy n
                    |> Bitwise.or carry
                    |> Bitwise.shiftRightZfBy 0
        in
        fromInts (Bitwise.shiftRightZfBy n higher) newLower


shiftRightBy63 : Int64 -> Int64
shiftRightBy63 (Int64 { higher }) =
    let
        onlyOnesOrZeros =
            Bitwise.shiftRightBy 31 higher
    in
    fromInts onlyOnesOrZeros onlyOnesOrZeros


and : Int -> Int64 -> Int64
and n (Int64 { lower }) =
    fromInts 0 (Bitwise.and n lower)


add : Int64 -> Int64 -> Int64
add (Int64 a) (Int64 p) =
    let
        lower =
            Bitwise.shiftRightZfBy 0 a.lower + Bitwise.shiftRightZfBy 0 p.lower

        higher =
            Bitwise.shiftRightZfBy 0 a.higher + Bitwise.shiftRightZfBy 0 p.higher
    in
    -- check for overflow in the lower bits
    if lower > 0xFFFFFFFF then
        Int64 { higher = Bitwise.shiftRightZfBy 0 (higher + 1), lower = Bitwise.shiftRightZfBy 0 lower }

    else
        Int64 { higher = Bitwise.shiftRightZfBy 0 higher, lower = Bitwise.shiftRightZfBy 0 lower }


negate : Int64 -> Int64
negate ((Int64 { higher, lower }) as int) =
    if lower == 0 && higher == 0 then
        int

    else
        add (fromInt 1) (fromInts (Bitwise.complement higher) (Bitwise.complement lower))


shiftLeftBy : Int -> Int64 -> Int64
shiftLeftBy n (Int64 { higher, lower }) =
    if n == 0 then
        Int64 { higher = higher, lower = lower }

    else if n > 32 then
        fromInts (Bitwise.shiftLeftBy n lower) 0

    else
        let
            carry =
                Bitwise.shiftRightZfBy (32 - n) lower

            newHigher =
                higher
                    |> Bitwise.shiftLeftBy n
                    |> Bitwise.or carry
        in
        fromInts newHigher (Bitwise.shiftLeftBy n lower)


xor : Int64 -> Int64 -> Int64
xor (Int64 a) (Int64 b) =
    fromInts
        (Bitwise.xor a.higher b.higher)
        (Bitwise.xor a.lower b.lower)


or : Int64 -> Int64 -> Int64
or (Int64 a) (Int64 b) =
    fromInts
        (Bitwise.or a.higher b.higher)
        (Bitwise.or a.lower b.lower)


{-| Convert a `Int` to `Int64`. This is guaranteed to work for integers in the [safe JS range](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER).

    fromInt 42
        |> toSignedString
        --> "42"

-}
fromInt53 : Int -> Int64
fromInt53 raw =
    if raw < 0 then
        Basics.negate raw
            |> fromInt53
            |> negate

    else if raw > 0xFFFFFFFF then
        Int64
            { higher = Bitwise.shiftRightZfBy 0 (raw // (2 ^ 32))
            , lower = Bitwise.shiftRightZfBy 0 raw
            }

    else
        Int64 { higher = 0, lower = raw }


toInt53 : Int64 -> Maybe Int
toInt53 ((Int64 { higher, lower }) as start) =
    let
        isPositive =
            Bitwise.and 0x80000000 higher == 0
    in
    if isPositive then
        if higher <= 0x00400000 then
            let
                lowerNorm =
                    if lower < 0 then
                        Bitwise.shiftRightZfBy 1 lower * 2 + Bitwise.and 1 lower

                    else
                        lower
            in
            Just <| (higher * 0x0000000100000000) + lowerNorm

        else
            Nothing

    else
        toInt53 (negate start) |> Maybe.map Basics.negate


toFloat : Int64 -> Float
toFloat ((Int64 { higher, lower }) as start) =
    let
        isPositive =
            Bitwise.and 0x80000000 higher == 0
    in
    if isPositive then
        let
            lowerNorm =
                if lower < 0 then
                    Bitwise.shiftRightZfBy 1 lower * 2 + Bitwise.and 1 lower

                else
                    lower
        in
        Basics.toFloat <|
            (higher * 0x0000000100000000)
                + lowerNorm

    else
        toFloat (negate start) |> Basics.negate
