module Bytes.Zigzag exposing (zag, zag64, zig, zig64)

import Avro.Internal.Int64 as Int64 exposing (Int64)
import Bitwise


zag : Int -> Int
zag n =
    Bitwise.xor
        (Bitwise.shiftRightZfBy 1 n)
        (negate (Bitwise.and n 0x01))


zig : Int -> Int
zig n =
    Bitwise.xor
        (Bitwise.shiftLeftBy 1 n)
        (Bitwise.shiftRightBy 31 n)


zag64 : Int64 -> Int64
zag64 n =
    Int64.xor
        (Int64.shiftRightZfBy 1 n)
        (Int64.negate (Int64.and 0x01 n))


zig64 : Int64 -> Int64
zig64 n =
    Int64.xor
        (Int64.shiftLeftBy 1 n)
        (Int64.shiftRightBy63 n)
