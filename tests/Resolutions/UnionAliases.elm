module Resolutions.UnionAliases exposing (..)

import Avro
import Avro.Codec as Codec exposing (Codec)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Test exposing (..)


compatible : Codec a -> Codec b -> a -> b -> Expect.Expectation
compatible reader writer expect written =
    case Avro.makeDecoder reader writer.schema of
        Err schemaError ->
            Expect.ok (Err schemaError)

        Ok decoder ->
            let
                encoded =
                    Avro.makeEncoder writer written
                        |> Encode.encode

                decoded =
                    Decode.decode decoder encoded
            in
            Expect.equal decoded (Just expect)


version1 : Codec { name : String }
version1 =
    Codec.success (\n -> { name = n })
        |> Codec.requiring "name" Codec.string .name
        |> Codec.record { baseName = "Person", nameSpace = [] }


version2 : Codec  { name : String, age : Maybe Int }
version2 =
    Codec.success (\n a -> { name = n, age = a })
        |> Codec.requiring "name" Codec.string .name
        |> Codec.optional "age" Codec.int .age
        |> Codec.record { baseName = "PersonV2", nameSpace = [] }
        |> Codec.withAliases [ { baseName = "Person", nameSpace = [] } ]


suite : Test
suite =
    describe "Union Schemas"
        [ describe "Union Schemas recursively permit aliases"
            [ test "From singular to union with alias" <|
                \_ -> compatible (Codec.maybe version2) version1 (Just { name = "Francis", age = Nothing }) { name = "Francis" }
            , test "From union to union with alias" <|
                \_ -> compatible (Codec.maybe version2) (Codec.maybe version1) (Just { name = "Francis", age = Nothing }) (Just { name = "Francis" })
            ]
        , describe "Simple types"
            [ test "Int to Option of Int" <|
                \_ -> compatible (Codec.maybe Codec.int) Codec.int (Just 10) 10
            , test "Int to Option of Long" <|
                \_ -> compatible (Codec.maybe Codec.long) Codec.int (Just 10) 10
            , test "Option Int to Option of Long" <|
                \_ -> compatible (Codec.maybe Codec.long) (Codec.maybe Codec.int) (Just 10) (Just 10)
            , test "Retain the correct branch from singular when promoting if possible on left" <|
                \_ -> compatible (Codec.union Codec.long Codec.int) Codec.int (Ok 10) 10
            , test "Retain the correct branch from singular when promoting if possible on right" <|
                \_ -> compatible (Codec.union Codec.int Codec.long) Codec.int (Err 10) 10
            , test "Retain the correct branch from unions when adjusting if possible on left" <|
                \_ -> compatible (Codec.union Codec.long (Codec.union Codec.int Codec.string)) (Codec.union Codec.int Codec.long) (Ok (Err 10)) (Err 10)
            ]
        ]
