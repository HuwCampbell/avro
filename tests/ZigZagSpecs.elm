module ZigZagSpecs exposing (suite)

import Bytes.Zigzag exposing (zag, zig)
import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "The ZigZag module"
        [ fuzz Fuzz.int "ZigZag encoding should round trip" <|
            \input ->
                input
                    |> zig
                    |> zag
                    |> Expect.equal input
        ]
