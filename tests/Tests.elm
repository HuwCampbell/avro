module Tests exposing (..)

import Avro.Internal.Parser as Internal
import Avro.Internal.ReadSchema as ReadSchema
import Avro.Name exposing (simpleName)
import Avro.Schema as Schema
import Avro.Value as Value
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Fuzz
import Test exposing (..)


encodeBytes : List Int -> Bytes
encodeBytes bs =
    List.map (\b -> Encode.unsignedInt8 b) bs
        |> Encode.sequence
        |> Encode.encode


simpleField : String -> Schema.Schema -> Schema.Field
simpleField n s =
    Schema.Field n [] Nothing Nothing s Nothing


exampleRecordSchema : Schema.Schema
exampleRecordSchema =
    Schema.Record
        { name = simpleName "example"
        , aliases = []
        , doc = Nothing
        , fields =
            [ simpleField "a" Schema.Long
            , simpleField "b" Schema.String
            ]
        }


flippedExampleRecordSchema : Schema.Schema
flippedExampleRecordSchema =
    Schema.Record
        { name = simpleName "example"
        , aliases = []
        , doc = Nothing
        , fields =
            [ simpleField "b" Schema.String
            , simpleField "a" Schema.Long
            ]
        }


exampleUnionSchema : Schema.Schema
exampleUnionSchema =
    Schema.Union
        { options =
            [ Schema.Null
            , Schema.String
            ]
        }


exampleArraySchema : Schema.Schema
exampleArraySchema =
    Schema.Array { items = Schema.Long }


readSchemaOf : Schema.Schema -> Maybe ReadSchema.ReadSchema
readSchemaOf s =
    Schema.deconflict s s


suite : Test
suite =
    describe "The Internal Parser module"
        [ describe "ZipZag"
            [ fuzz Fuzz.int "Zigzag encoding should return the right results" <|
                \input ->
                    input
                        |> Internal.zig
                        |> Internal.zag
                        |> Expect.equal input
            ]
        , describe "decode var int"
            [ test "decodes simple single bytes" <|
                \_ ->
                    let
                        parser =
                            Internal.makeDecoder ReadSchema.Int

                        input =
                            encodeBytes [ 0x04 ]

                        result =
                            Decode.decode parser input
                    in
                    Expect.equal result (Just <| Value.Int 2)
            ]
        , describe "decode union"
            [ test "read schema emerges" <|
                \_ ->
                    let
                        schemata =
                            readSchemaOf exampleUnionSchema
                    in
                    Expect.equal schemata (Just <| ReadSchema.Union { options = [ ( 0, ReadSchema.Null ), ( 1, ReadSchema.String ) ] })
            , test "decodes null from exaple" <|
                \_ ->
                    let
                        parser =
                            readSchemaOf exampleUnionSchema
                                |> Maybe.map Internal.makeDecoder

                        input =
                            encodeBytes [ 0x00 ]

                        result =
                            parser
                                |> Maybe.andThen (\p -> Decode.decode p input)
                    in
                    Expect.equal result (Just <| Value.Union 0 Value.Null)
            , test "decodes string from exaple" <|
                \_ ->
                    let
                        parser =
                            readSchemaOf exampleUnionSchema
                                |> Maybe.map Internal.makeDecoder

                        input =
                            encodeBytes [ 0x02, 0x02, 0x61 ]

                        result =
                            parser
                                |> Maybe.andThen (\p -> Decode.decode p input)
                    in
                    Expect.equal result (Just <| Value.Union 1 (Value.String "a"))
            ]
        , describe "records"
            [ test "decodes the example" <|
                \_ ->
                    let
                        parser =
                            readSchemaOf exampleRecordSchema
                                |> Maybe.map Internal.makeDecoder

                        input =
                            encodeBytes [ 0x36, 0x06, 0x66, 0x6F, 0x6F ]

                        result =
                            parser
                                |> Maybe.andThen (\p -> Decode.decode p input)

                        expected =
                            Value.Record (simpleName "example") [ Value.Long 27, Value.String "foo" ]
                    in
                    Expect.equal result (Just expected)
            , test "decodes the example with flipped order" <|
                \_ ->
                    let
                        parser =
                            Schema.deconflict exampleRecordSchema flippedExampleRecordSchema
                                |> Maybe.map Internal.makeDecoder

                        input =
                            encodeBytes [ 0x06, 0x66, 0x6F, 0x6F, 0x36 ]

                        result =
                            parser
                                |> Maybe.andThen (\p -> Decode.decode p input)

                        expected =
                            Value.Record (simpleName "example") [ Value.Long 27, Value.String "foo" ]
                    in
                    Expect.equal result (Just expected)
            ]
        , describe "arrays"
            [ test "read schema emerges" <|
                \_ ->
                    let
                        schemata =
                            readSchemaOf exampleArraySchema
                    in
                    Expect.equal schemata (Just <| ReadSchema.Array { items = ReadSchema.Long })
            , test "decodes the example" <|
                \_ ->
                    let
                        parser =
                            readSchemaOf exampleArraySchema
                                |> Maybe.map Internal.makeDecoder

                        input =
                            encodeBytes [ 0x04, 0x06, 0x36, 0x00 ]

                        result =
                            parser
                                |> Maybe.andThen (\p -> Decode.decode p input)

                        expected =
                            Value.Array [ Value.Long 3, Value.Long 27 ]
                    in
                    Expect.equal result (Just expected)
            ]
        ]
