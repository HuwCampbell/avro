module ZigZag exposing (suite)

import Avro.Internal.Int64 as Int64
import Bytes.Zigzag exposing (zag, zag64, zig, zig64)
import Expect
import Fuzz
import Test exposing (..)


bigger : Fuzz.Fuzzer Int
bigger =
    Fuzz.intRange (-2 ^ 52) (2 ^ 52)


trip64 : Int -> Expect.Expectation
trip64 input =
    Int64.fromInt53 input
        |> zig64
        |> zag64
        |> Int64.toInt53
        |> Expect.equal (Just input)


suite : Test
suite =
    describe "The ZigZag module"
        [ fuzz Fuzz.int "ZigZag encoding should round trip" <|
            \input ->
                input
                    |> zig
                    |> zag
                    |> Expect.equal input
        , fuzz bigger
            "ZigZag64 encoding should round trip"
            trip64
        , describe "ZigZag64 examples"
            [ test "zero" <|
                \_ -> trip64 0
            , test "small positive" <|
                \_ -> trip64 10
            , test "small negative" <|
                \_ -> trip64 -10
            , test "large positive" <|
                \_ -> trip64 4503599627370496
            , test "large negative" <|
                \_ -> trip64 -4503599627370496
            , test "edge" <|
                \_ -> trip64 4294967296
            , test "edge minus 1" <|
                \_ -> trip64 4294967295
            , test "edge plus 1" <|
                \_ -> trip64 4294967297
            ]
        ]
