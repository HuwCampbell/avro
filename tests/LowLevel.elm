module LowLevel exposing (suite)

import Avro.Deconflict as Deconflict
import Avro.Internal.Bytes as Internal
import Avro.Name exposing (TypeName)
import Avro.ReadSchema as ReadSchema
import Avro.Schema as Schema
import Avro.Value as Value
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Expect
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
        { name = TypeName "example" []
        , aliases = []
        , doc = Nothing
        , fields =
            [ simpleField "a" (Schema.Long { logicalType = Nothing })
            , simpleField "b" (Schema.String { logicalType = Nothing })
            ]
        }


flippedExampleRecordSchema : Schema.Schema
flippedExampleRecordSchema =
    Schema.Record
        { name = TypeName "example" []
        , aliases = []
        , doc = Nothing
        , fields =
            [ simpleField "b" (Schema.String { logicalType = Nothing })
            , simpleField "a" (Schema.Long { logicalType = Nothing })
            ]
        }


exampleUnionSchema : Schema.Schema
exampleUnionSchema =
    Schema.Union
        { options =
            [ Schema.Null
            , Schema.String { logicalType = Nothing }
            ]
        }


exampleArraySchema : Schema.Schema
exampleArraySchema =
    Schema.Array { items = Schema.Long { logicalType = Nothing } }


readSchemaOf : Schema.Schema -> Maybe ReadSchema.ReadSchema
readSchemaOf s =
    Result.toMaybe <| Deconflict.deconflict s s


suite : Test
suite =
    describe "The Internal Parser module"
        [ describe "decode var int"
            [ test "decodes simple single bytes" <|
                \_ ->
                    let
                        parser =
                            Internal.makeDecoder Dict.empty ReadSchema.Int

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
                                |> Maybe.map (Internal.makeDecoder Dict.empty)

                        result =
                            parser
                                |> Maybe.andThen
                                    (\p ->
                                        let
                                            input =
                                                encodeBytes [ 0x00 ]
                                        in
                                        Decode.decode p input
                                    )
                    in
                    Expect.equal result (Just <| Value.Union 0 Value.Null)
            , test "decodes string from exaple" <|
                \_ ->
                    let
                        parser =
                            readSchemaOf exampleUnionSchema
                                |> Maybe.map (Internal.makeDecoder Dict.empty)

                        result =
                            parser
                                |> Maybe.andThen
                                    (\p ->
                                        let
                                            input =
                                                encodeBytes [ 0x02, 0x02, 0x61 ]
                                        in
                                        Decode.decode p input
                                    )
                    in
                    Expect.equal result (Just <| Value.Union 1 (Value.String "a"))
            ]
        , describe "records"
            [ test "decodes the example" <|
                \_ ->
                    let
                        parser =
                            readSchemaOf exampleRecordSchema
                                |> Maybe.map (Internal.makeDecoder Dict.empty)

                        result =
                            parser
                                |> Maybe.andThen
                                    (\p ->
                                        let
                                            input =
                                                encodeBytes [ 0x36, 0x06, 0x66, 0x6F, 0x6F ]
                                        in
                                        Decode.decode p input
                                    )

                        expected =
                            Value.Record [ Value.Long 27, Value.String "foo" ]
                    in
                    Expect.equal result (Just expected)
            , test "decodes the example with flipped order" <|
                \_ ->
                    let
                        parser =
                            Deconflict.deconflict exampleRecordSchema flippedExampleRecordSchema
                                |> Result.map (Internal.makeDecoder Dict.empty)

                        result =
                            Result.toMaybe parser
                                |> Maybe.andThen
                                    (\p ->
                                        let
                                            input =
                                                encodeBytes [ 0x06, 0x66, 0x6F, 0x6F, 0x36 ]
                                        in
                                        Decode.decode p input
                                    )

                        expected =
                            Value.Record [ Value.Long 27, Value.String "foo" ]
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
                                |> Maybe.map (Internal.makeDecoder Dict.empty)

                        result =
                            parser
                                |> Maybe.andThen
                                    (\p ->
                                        let
                                            input =
                                                encodeBytes [ 0x04, 0x06, 0x36, 0x00 ]
                                        in
                                        Decode.decode p input
                                    )

                        expected =
                            Value.Array [ Value.Long 3, Value.Long 27 ]
                    in
                    Expect.equal result (Just expected)
            ]
        ]
