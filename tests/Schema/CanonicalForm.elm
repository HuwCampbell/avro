module Schema.CanonicalForm exposing (suite)

import Avro
import Avro.Schema as Schema
import Expect
import Json.Decode
import Json.Encode
import Test exposing (..)


type alias Example =
    { expanded : String, canonical : String }



-- | This example is kind of pulled from the rust implementation,
--   but, their result is actually incorrect and nests the last
--   long type.


rustSpecExample : Example
rustSpecExample =
    { expanded =
        """
        {
          "type": "record",
          "name": "test",
          "fields": [
            {"name": "a", "type": "long", "default": 42, "doc": "The field a"},
            {"name": "b", "type": "string", "namespace": "test.a"},
            {"name": "c", "type": "long", "logicalType": "timestamp-micros"}
          ]
        }
        """
    , canonical =
        """{"name":"test","type":"record","fields":[{"name":"a","type":"long"},{"name":"b","type":"string"},{"name":"c","type":"long"}]}"""
    }



-- These are all pulled from the Apache avro
-- schema-tests.txt file


test000 : Example
test000 =
    { expanded = "\"null\""
    , canonical = "\"null\""
    }


test001 : Example
test001 =
    { expanded = """{"type":"null"}"""
    , canonical = "\"null\""
    }


test002 : Example
test002 =
    { expanded = "\"boolean\""
    , canonical = "\"boolean\""
    }


test003 : Example
test003 =
    { expanded = """ {"type":"boolean"}"""
    , canonical = "\"boolean\""
    }


test004 : Example
test004 =
    { expanded = "\"int\""
    , canonical = "\"int\""
    }


test005 : Example
test005 =
    { expanded = """{"type":"int"}"""
    , canonical = "\"int\""
    }


test006 : Example
test006 =
    { expanded = "\"long\""
    , canonical = "\"long\""
    }


test007 : Example
test007 =
    { expanded = """{"type":"long"}"""
    , canonical = "\"long\""
    }


test008 : Example
test008 =
    { expanded = "\"float\""
    , canonical = "\"float\""
    }


test009 : Example
test009 =
    { expanded = """{"type":"float"}"""
    , canonical = "\"float\""
    }


test010 : Example
test010 =
    { expanded = "\"double\""
    , canonical = "\"double\""
    }


test011 : Example
test011 =
    { expanded = """{"type":"double"}"""
    , canonical = "\"double\""
    }


test012 : Example
test012 =
    { expanded = "\"bytes\""
    , canonical = "\"bytes\""
    }


test013 : Example
test013 =
    { expanded = """{"type":"bytes"}"""
    , canonical = "\"bytes\""
    }


test014 : Example
test014 =
    { expanded = "\"string\""
    , canonical = "\"string\""
    }


test015 : Example
test015 =
    { expanded = """{"type":"string"}"""
    , canonical = "\"string\""
    }


test016 : Example
test016 =
    { expanded = "[  ]"
    , canonical = "[]"
    }


test017 : Example
test017 =
    { expanded = """[ "int"  ]"""
    , canonical = """["int"]"""
    }


test018 : Example
test018 =
    { expanded = """[ "int" , {"type":"boolean"} ]"""
    , canonical = """["int","boolean"]"""
    }


test019 : Example
test019 =
    { expanded = """{"fields":[], "type":"record", "name":"foo"}"""
    , canonical = """{"name":"foo","type":"record","fields":[]}"""
    }


test020 : Example
test020 =
    { expanded = """{"fields":[], "type":"record", "name":"foo", "namespace":"x.y"}"""
    , canonical = """{"name":"x.y.foo","type":"record","fields":[]}"""
    }


test021 : Example
test021 =
    { expanded = """{"fields":[], "type":"record", "name":"a.b.foo", "namespace":"x.y"}"""
    , canonical = """{"name":"a.b.foo","type":"record","fields":[]}"""
    }


test022 : Example
test022 =
    { expanded = """{"fields":[], "type":"record", "name":"foo", "doc":"Useful info"}"""
    , canonical = """{"name":"foo","type":"record","fields":[]}"""
    }


test023 : Example
test023 =
    { expanded = """{"fields":[], "type":"record", "name":"foo", "aliases":["foo","bar"]}"""
    , canonical = """{"name":"foo","type":"record","fields":[]}"""
    }


test024 : Example
test024 =
    { expanded = """{"fields":[], "type":"record", "name":"foo", "doc":"foo", "aliases":["foo","bar"]}"""
    , canonical = """{"name":"foo","type":"record","fields":[]}"""
    }


test025 : Example
test025 =
    { expanded = """{"fields":[{"type":{"type":"boolean"}, "name":"f1"}], "type":"record", "name":"foo"}"""
    , canonical = """{"name":"foo","type":"record","fields":[{"name":"f1","type":"boolean"}]}"""
    }


test026 : Example
test026 =
    { expanded =
        """
        { "fields":[{"type":"boolean", "aliases":[], "name":"f1", "default":true},
        {"order":"descending","name":"f2","doc":"Hello","type":"int"}],
        "type":"record", "name":"foo"
        }
        """
    , canonical = """{"name":"foo","type":"record","fields":[{"name":"f1","type":"boolean"},{"name":"f2","type":"int"}]}"""
    }


test027 : Example
test027 =
    { expanded = """{"type":"enum", "name":"foo", "symbols":["A1"]}"""
    , canonical = """{"name":"foo","type":"enum","symbols":["A1"]}"""
    }


test028 : Example
test028 =
    { expanded = """{"namespace":"x.y.z", "type":"enum", "name":"foo", "doc":"foo bar", "symbols":["A1", "A2"]}"""
    , canonical = """{"name":"x.y.z.foo","type":"enum","symbols":["A1","A2"]}"""
    }


test029 : Example
test029 =
    { expanded = """{"name":"foo","type":"fixed","size":15}"""
    , canonical = """{"name":"foo","type":"fixed","size":15}"""
    }


test030 : Example
test030 =
    { expanded = """{"namespace":"x.y.z", "type":"fixed", "name":"foo", "doc":"foo bar", "size":32}"""
    , canonical = """{"name":"x.y.z.foo","type":"fixed","size":32}"""
    }


test031 : Example
test031 =
    { expanded = """{ "items":{"type":"null"}, "type":"array"}"""
    , canonical = """{"type":"array","items":"null"}"""
    }


test032 : Example
test032 =
    { expanded = """{ "values":"string", "type":"map"}"""
    , canonical = """{"type":"map","values":"string"}"""
    }


test033 : Example
test033 =
    { expanded = """{"name":"PigValue","type":"record",
                    "fields":[{"name":"value", "type":["null", "int", "long", "PigValue"]}]}"""
    , canonical = """{"name":"PigValue","type":"record","fields":[{"name":"value","type":["null","int","long","PigValue"]}]}"""
    }


testExample : Example -> Expect.Expectation
testExample { expanded, canonical } =
    let
        decoded =
            Json.Decode.decodeString Avro.schemaDecoder expanded
                |> Result.map Schema.canonicalise
                |> Result.map (Json.Encode.encode 0 << Avro.schemaEncoder)
    in
    Expect.equal decoded (Ok <| canonical)


suite : Test
suite =
    describe "Canonicalize"
        [ test "Canonical form example is correct" <|
            \_ -> testExample rustSpecExample
        , test "Java test 000" <|
            \_ -> testExample test000
        , test "Java test 001" <|
            \_ -> testExample test001
        , test "Java test 002" <|
            \_ -> testExample test002
        , test "Java test 003" <|
            \_ -> testExample test003
        , test "Java test 004" <|
            \_ -> testExample test004
        , test "Java test 005" <|
            \_ -> testExample test005
        , test "Java test 006" <|
            \_ -> testExample test006
        , test "Java test 007" <|
            \_ -> testExample test007
        , test "Java test 008" <|
            \_ -> testExample test008
        , test "Java test 009" <|
            \_ -> testExample test009
        , test "Java test 010" <|
            \_ -> testExample test010
        , test "Java test 011" <|
            \_ -> testExample test011
        , test "Java test 012" <|
            \_ -> testExample test012
        , test "Java test 013" <|
            \_ -> testExample test013
        , test "Java test 014" <|
            \_ -> testExample test014
        , test "Java test 015" <|
            \_ -> testExample test015
        , test "Java test 016" <|
            \_ -> testExample test016
        , test "Java test 017" <|
            \_ -> testExample test017
        , test "Java test 018" <|
            \_ -> testExample test018
        , test "Java test 019" <|
            \_ -> testExample test019
        , test "Java test 020" <|
            \_ -> testExample test020
        , test "Java test 021" <|
            \_ -> testExample test021
        , test "Java test 022" <|
            \_ -> testExample test022
        , test "Java test 023" <|
            \_ -> testExample test023
        , test "Java test 024" <|
            \_ -> testExample test024
        , test "Java test 025" <|
            \_ -> testExample test025
        , test "Java test 026" <|
            \_ -> testExample test026
        , test "Java test 027" <|
            \_ -> testExample test027
        , test "Java test 028" <|
            \_ -> testExample test028
        , test "Java test 029" <|
            \_ -> testExample test029
        , test "Java test 030" <|
            \_ -> testExample test030
        , test "Java test 031" <|
            \_ -> testExample test031
        , test "Java test 032" <|
            \_ -> testExample test032
        , test "Java test 033" <|
            \_ -> testExample test033
        ]
