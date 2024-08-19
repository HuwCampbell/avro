module JsonEncodings exposing (suite)

import Avro.Json.Schema exposing (decodeSchema, encodeSchema)
import Avro.Json.Value as Avro
import Avro.Schema as Schema exposing (Schema)
import Avro.Value as Avro
import Expect
import Generators
import Json.Decode exposing (Decoder, decodeValue)
import Json.Encode exposing (Value)
import Test exposing (..)


example1 : String
example1 =
    """
    {
      "type": "record",
      "name": "test",
      "fields" : [
        {"name": "a", "type": "long"},
        {"name": "b", "type": "string"}
      ]
    }
    """


example1Expected : Schema
example1Expected =
    Schema.Record
        { name = { baseName = "test", nameSpace = [] }
        , aliases = []
        , doc = Nothing
        , fields =
            [ { aliases = []
              , default = Nothing
              , doc = Nothing
              , name = "a"
              , order = Nothing
              , type_ = Schema.Long { logicalType = Nothing }
              }
            , { aliases = []
              , default = Nothing
              , doc = Nothing
              , name = "b"
              , order = Nothing
              , type_ = Schema.String { logicalType = Nothing }
              }
            ]
        }


example2 : String
example2 =
    """{"type": "enum", "name": "Foo", "symbols": ["A", "B", "C", "D"] }"""


example2Expected : Schema
example2Expected =
    Schema.Enum
        { name = { baseName = "Foo", nameSpace = [] }
        , aliases = []
        , doc = Nothing
        , symbols = [ "A", "B", "C", "D" ]
        , default = Nothing
        }


example3 : String
example3 =
    """{"type": "array", "items": "long"}"""


example3Expected : Schema
example3Expected =
    Schema.Array { items = Schema.Long { logicalType = Nothing } }


{-| This example is from the Rust schema definitions
-}
example4 : String
example4 =
    """{"type": {"type": "string"}}"""


example4Expected : Schema
example4Expected =
    Schema.String { logicalType = Nothing }


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


tripValueWithSchema : ( Schema, Avro.Value ) -> Expect.Expectation
tripValueWithSchema ( s, v ) =
    tripper (Avro.decodeValue s) (Avro.encodeValue s) v


testExample : String -> Schema -> Expect.Expectation
testExample example expected =
    let
        decoded =
            Json.Decode.decodeString decodeSchema example
    in
    Expect.equal decoded (Ok <| expected)


suite : Test
suite =
    describe "Json encoding"
        [ test "Record example " <|
            \_ -> testExample example1 example1Expected
        , test "Enum example " <|
            \_ -> testExample example2 example2Expected
        , test "Array example " <|
            \_ -> testExample example3 example3Expected
        , test "Recursive Schema example " <|
            \_ -> testExample example4 example4Expected
        , fuzz (Generators.fuzzSchema 3) "Schema should roundtrip" <|
            tripSchema
        , fuzz Generators.fuzzSchemaAndValue "Values should roundtrip" <|
            tripValueWithSchema
        ]
