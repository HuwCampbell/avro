module Resolution.Basics exposing (..)

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


version2 : Codec { name : String, age : Maybe Int }
version2 =
    Codec.success (\n a -> { name = n, age = a })
        |> Codec.requiring "name" Codec.string .name
        |> Codec.optional "age" Codec.int .age
        |> Codec.record { baseName = "PersonV2", nameSpace = [] }
        |> Codec.withAliases [ { baseName = "Person", nameSpace = [] } ]


suite : Test
suite =
    describe "Schema deconflicting module"
        [ describe "Basic types and unions"
            [ test "Int to Long" <|
                \_ -> compatible Codec.long Codec.int 10 10
            , test "Int to Float" <|
                \_ -> compatible Codec.float32 Codec.long 10 10
            , test "Int to Double" <|
                \_ -> compatible Codec.float64 Codec.int 10 10
            ]
        , describe "Records"
            [ test "Record with new name (and alias to old) and new optional field" <|
                \_ -> compatible version2 version1 { name = "Francis", age = Nothing } { name = "Francis" }
            ]
        ]
