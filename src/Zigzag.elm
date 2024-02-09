module Zigzag exposing (..)

import Bitwise


zag : Int -> Int
zag n =
    Bitwise.xor
        (Bitwise.shiftRightZfBy 1 n)
        (negate (Bitwise.and n 0x01))


zig : Int -> Int
zig n =
    if n >= 0 then
        2 * n

    else
        2 * abs n - 1
