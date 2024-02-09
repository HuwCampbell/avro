module ZigZagSpecs exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import Zigzag exposing (zag, zig)


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
