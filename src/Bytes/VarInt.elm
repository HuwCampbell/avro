module Bytes.VarInt exposing (getZigZag, getZigZag64, putZigZag, putZigZag64)

import Avro.Internal.Int64 as Int64 exposing (Int64)
import Bitwise
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Bytes.Zigzag exposing (zag, zag64, zig, zig64)


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


getVarInt64 : Decoder Int64
getVarInt64 =
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
                                Int64.shiftLeftBy (7 * depth) (Int64.fromSmallPositiveInt dataBits)
                                    |> Int64.or acc
                        in
                        if top == 0 then
                            Decode.Done updated

                        else
                            Decode.Loop ( depth + 1, updated )
                    )
    in
    Decode.loop ( 0, Int64.fromSmallPositiveInt 0 ) step


getZigZag64 : Decoder Int64.Int64
getZigZag64 =
    getVarInt64
        |> Decode.map zag64


putVarInt64 : Int64.Int64 -> Encoder
putVarInt64 =
    let
        go lower n =
            let
                ( base, top ) =
                    Int64.popBase128 n

                finish =
                    top == Int64.fromSmallPositiveInt 0

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


putZigZag64 : Int64.Int64 -> Encoder
putZigZag64 =
    zig64 >> putVarInt64
