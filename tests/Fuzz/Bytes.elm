module Fuzz.Bytes exposing (bytes)

import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Fuzz


bytes : Fuzz.Fuzzer Bytes
bytes =
    Fuzz.list (Fuzz.intRange 0 255)
        |> Fuzz.map (List.map Encode.unsignedInt8 >> Encode.sequence >> Encode.encode)
