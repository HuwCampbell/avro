module ZigZagSpecs exposing (suite)

import Bytes.Zigzag exposing (zag, zig)
import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "The Zigzag Parser module"
        [ fuzz Fuzz.int "Zigzag encoding should round trip" <|
            \input ->
                input
                    |> zig
                    |> zag
                    |> Expect.equal input
        ]
