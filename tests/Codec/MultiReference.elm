module Codec.MultiReference exposing (suite)

import Avro
import Avro.Codec exposing (..)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Json.Decode
import Test exposing (..)



--
-- This schema was tested manually with the Java implementation.
-- We're testing that the name 'XY' can be used later in the
-- schema definition.


referenceSchema : String
referenceSchema =
    """
    { "name": "top",
      "type": "record",
      "fields": [ { "name": "a", "type": { "type": "record",
                                          "name": "XY",
                                          "fields": [ { "name": "q", "type": "int" } ]
                                        }
                  }
                , { "name": "b", "type": "XY" }
                ]
    }
    """


type alias XY =
    { q : Int }


type alias Top =
    { a : XY, b : XY }


xyCodec : Codec XY
xyCodec =
    success XY
        |> requiring "q" int .q
        |> record { baseName = "XY", nameSpace = [] }


topCodec : Codec Top
topCodec =
    success Top
        |> requiring "a" xyCodec .a
        |> requiring "b" (namedType xyCodec) .b
        |> record { baseName = "top", nameSpace = [] }


trip : Codec a -> a -> Expect.Expectation
trip codec example =
    tripVersions identity codec codec example


tripVersions : (a -> b) -> Codec b -> Codec a -> a -> Expect.Expectation
tripVersions inject reader writer example =
    let
        decoder =
            Avro.makeDecoder reader writer.schema

        decoded =
            Result.toMaybe decoder
                |> Maybe.andThen
                    (\d ->
                        let
                            encoder =
                                Avro.makeEncoder writer

                            encoded =
                                encoder example
                                    |> Encode.encode
                        in
                        Decode.decode d encoded
                    )
    in
    Expect.equal decoded (Just <| inject example)


example1 : Top
example1 =
    Top (XY 4) (XY 5)


schemaMatches : Expect.Expectation
schemaMatches =
    let
        reference =
            Json.Decode.decodeString Avro.schemaDecoder referenceSchema
    in
    Expect.equal reference <|
        Ok topCodec.schema


suite : Test
suite =
    describe "Named schema resolutions from within the schema."
        [ test "codec should match Java example." <|
            \_ -> schemaMatches
        , test "should parse and decode when a named type is defined in an earlier field." <|
            \_ -> trip topCodec example1
        ]
