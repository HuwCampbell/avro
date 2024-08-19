module BinaryEncodings exposing (suite)

import Avro
import Avro.Codec exposing (Codec)
import Avro.Schema exposing (Schema)
import Avro.Value as Avro
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Generators
import Test exposing (..)


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


tripValueWithSchema : ( Schema, Avro.Value ) -> Expect.Expectation
tripValueWithSchema ( schema, value ) =
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
