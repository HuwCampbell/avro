module Bytes.DecodeExtra exposing (..)

import Bytes.Decode as Decode exposing (Decoder)


lazy : (() -> Decoder a) -> Decoder a
lazy self =
    Decode.succeed ()
        |> Decode.andThen self
