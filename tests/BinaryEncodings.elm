module BinaryEncodings exposing (suite)

import Avro.Json.Schema exposing (decodeSchema, encodeSchema)
import Avro.Json.Value as Avro
import Avro.Schema exposing (Schema, SortOrder(..))
import Avro.Value as Avro
import Avro as Avro
import Expect
import Generators
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Test exposing (..)
import Avro.Codec exposing (Codec)



trip : Codec a -> a -> Expect.Expectation
trip codec example =
    tripVersions codec codec example


tripVersions : Codec a -> Codec a -> a -> Expect.Expectation
tripVersions reader writer example =
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
    Expect.equal decoded (Just <| example)

tripValueWithSchema : (Schema, Avro.Value) -> Expect.Expectation
tripValueWithSchema (schema, value) =
    let
        dynamicCodec =
            Codec schema Just identity

    in
    trip dynamicCodec value


suite : Test
suite =
    describe "Binary encoding"
        [ fuzz Generators.fuzzSchemaAndValue "Values should roundtrip" <|
            tripValueWithSchema
        ]
