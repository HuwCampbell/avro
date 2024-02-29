module Issues.Avro3152 exposing (suite)

import Avro.Json.Schema exposing (decodeSchema, encodeSchema)
import Avro.Schema as Schema exposing (Schema)
import Expect
import Json.Decode exposing (Decoder, decodeValue)
import Json.Encode exposing (Value)
import Test exposing (..)


issueJson : String
issueJson =
    """
    {
      "type": "record",
      "namespace": "much.namespace",
      "name": "ModernRecord",
      "fields": [
        {
          "name" : "enumField",
          "type" : {
            "type" : "enum",
            "name" : "ModernEnum",
            "symbols" : [ "THE", "SPEC", "IS", "A", "LIE" ],
            "aliases": [
              ".AncientEnum"
            ]
          }
        }
      ],
      "aliases": [
        ".AncientSchema"
      ]
    }
    """


issueExpected : Schema
issueExpected =
    Schema.Record
        { name = { baseName = "ModernRecord", nameSpace = [ "much", "namespace" ] }
        , aliases = [ { baseName = "AncientSchema", nameSpace = [] } ]
        , doc = Nothing
        , fields =
            [ { name = "enumField"
              , default = Nothing
              , doc = Nothing
              , aliases = []
              , order = Nothing
              , type_ =
                    Schema.Enum
                        { name = { baseName = "ModernEnum", nameSpace = [ "much", "namespace" ] }
                        , aliases = [ { baseName = "AncientEnum", nameSpace = [] } ]
                        , default = Nothing
                        , doc = Nothing
                        , symbols = [ "THE", "SPEC", "IS", "A", "LIE" ]
                        }
              }
            ]
        }


testExample : String -> Schema -> Expect.Expectation
testExample example expected =
    let
        decoded =
            Json.Decode.decodeString decodeSchema example
    in
    Expect.equal decoded (Ok <| expected)


tripper : Decoder a -> (a -> Value) -> a -> Expect.Expectation
tripper decoder encoder example =
    let
        encoded =
            encoder example

        decoded =
            decodeValue decoder encoded
    in
    Expect.equal decoded (Ok <| example)


tripSchema : Schema -> Expect.Expectation
tripSchema =
    tripper decodeSchema encodeSchema


suite : Test
suite =
    describe "Issue 3152"
        [ test "Null namespaces aliases can be parsed" <|
            \_ -> testExample issueJson issueExpected
        , test "Null namespaces aliases can be tripped" <|
            \_ -> tripSchema issueExpected
        ]
