module Avro.Json.Schema exposing (..)

import Array
import Avro.Schema exposing (Schema(..))
import Json.Encode as Json exposing (..)


encodeSchema : Schema -> Value
encodeSchema s =
    case s of
        Null ->
            string "null"

        Boolean ->
            string "boolean"

        Int ->
            string "int"

        Long ->
            string "long"

        Float ->
            string "float"

        Double ->
            string "double"

        Bytes ->
            string "bytes"

        String ->
            string "string"

        Array info ->
            object
                [ ( "type", string "array" )
                , ( "items", encodeSchema info.item )
                ]

        Map info ->
            object
                [ ( "type", string "map" )
                , ( "items", encodeSchema info.values )
                ]

        Union info ->
            Array.fromList info.options
                |> array encodeSchema

        _ ->
            null
