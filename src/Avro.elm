module Avro exposing (..)

import Avro.Codec
import Avro.Json.Schema
import Avro.Schema exposing (Schema(..))
import Avro.Value exposing (Value(..))


type alias Schema =
    Avro.Schema.Schema


type alias Value =
    Avro.Value.Value
