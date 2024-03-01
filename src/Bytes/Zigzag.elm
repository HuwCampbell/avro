module Bytes.Zigzag exposing (zag, zig)

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
