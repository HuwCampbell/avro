module JsonSpecs exposing (..)

import Avro.Codec exposing (..)
import Avro.Json.Schema exposing (decodeSchema, encodeSchema)
import Avro.Name exposing (TypeName)
import Avro.Schema as Schema exposing (Field, Schema)
import Expect
import Fuzz
import Json.Decode exposing (Decoder, decodeValue)
import Json.Encode exposing (Value)
import Test exposing (..)


tripper : Decoder a -> (a -> Value) -> a -> Expect.Expectation
tripper decoder encoder example =
    let
        encoded =
            encoder example

        decoded =
            decodeValue decoder encoded
    in
    Expect.equal decoded (Ok <| example)


trip : Schema -> Expect.Expectation
trip =
    tripper decodeSchema encodeSchema


fuzzName : Fuzz.Fuzzer TypeName
fuzzName =
    Fuzz.map
        (\n -> TypeName n [])
        Fuzz.string


fuzzField : Int -> Fuzz.Fuzzer Field
fuzzField i =
    Fuzz.map6
        Field
        Fuzz.string
        (Fuzz.list Fuzz.string)
        (Fuzz.constant Nothing)
        (Fuzz.constant Nothing)
        (Fuzz.lazy (\_ -> fuzzSchema i))
        (Fuzz.constant Nothing)


fuzzSchema : Int -> Fuzz.Fuzzer Schema
fuzzSchema i =
    let
        base =
            [ Fuzz.constant Schema.Null
            , Fuzz.constant Schema.Boolean
            , Fuzz.constant Schema.Int
            , Fuzz.constant Schema.Long
            , Fuzz.constant Schema.Float
            , Fuzz.constant Schema.Double
            , Fuzz.constant Schema.Bytes
            , Fuzz.constant Schema.String
            ]

        compound =
            [ Fuzz.map
                (\items -> Schema.Array { items = items })
                (Fuzz.lazy (\_ -> fuzzSchema (i - 1)))
            , Fuzz.map
                (\values -> Schema.Map { values = values })
                (Fuzz.lazy (\_ -> fuzzSchema (i - 1)))
            , Fuzz.map3
                (\name aliases fields -> Schema.Record { name = name, aliases = aliases, fields = fields, doc = Nothing })
                fuzzName
                (Fuzz.list fuzzName)
                (Fuzz.listOfLengthBetween 1 4 (Fuzz.lazy (\_ -> fuzzField (i - 1))))
            , Fuzz.list
                (Fuzz.lazy (\_ -> fuzzSchema (i - 1)))
                |> Fuzz.map (\options -> Schema.Union { options = options })
            , Fuzz.map3
                (\name aliases symbols -> Schema.Enum { name = name, aliases = aliases, symbols = symbols, doc = Nothing })
                fuzzName
                (Fuzz.list fuzzName)
                (Fuzz.list Fuzz.string)
            , Fuzz.map3
                (\name aliases size -> Schema.Fixed { name = name, aliases = aliases, size = size })
                fuzzName
                (Fuzz.list fuzzName)
                Fuzz.int
            ]
    in
    Fuzz.oneOf
        (if i > 0 then
            List.append base compound

         else
            base
        )


suite : Test
suite =
    describe "Json encoding"
        [ fuzz (fuzzSchema 3) "Schema should roundtrip" <|
            trip
        ]
