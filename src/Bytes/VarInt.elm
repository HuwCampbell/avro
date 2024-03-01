module Bytes.VarInt exposing (getZigZag, putZigZag)

import Bitwise
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Bytes.Zigzag exposing (zag, zig)


getZigZag : Decoder Int
getZigZag =
    getVarInt
        |> Decode.map zag


getVarInt : Decoder Int
getVarInt =
    let
        step ( depth, acc ) =
            Decode.unsignedInt8
                |> Decode.map
                    (\b ->
                        let
                            top =
                                Bitwise.and b 0x80

                            dataBits =
                                Bitwise.and b 0x7F

                            updated =
                                Bitwise.shiftLeftBy (7 * depth) dataBits
                                    |> Bitwise.or acc
                        in
                        if top == 0 then
                            Decode.Done updated

                        else
                            Decode.Loop ( depth + 1, updated )
                    )
    in
    Decode.loop ( 0, 0 ) step


putZigZag : Int -> Encoder
putZigZag =
    zig >> putVarInt


putVarInt : Int -> Encoder
putVarInt =
    let
        go lower n =
            let
                top =
                    Bitwise.shiftRightZfBy 7 n

                finish =
                    top == 0

                base =
                    Bitwise.and n 0x7F

                encoded =
                    Encode.unsignedInt8
                        (if finish then
                            base

                         else
                            Bitwise.or base 0x80
                        )
            in
            if finish then
                Encode.sequence (List.reverse (encoded :: lower))

            else
                go (encoded :: lower) top
    in
    go []
