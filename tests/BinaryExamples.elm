module BinaryExamples exposing (suite)

import Avro
import Avro.Codec as Codec exposing (..)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode
import Expect
import Test exposing (..)


type alias TestRecord =
    { a : Int, b : String }


testCodec : Codec TestRecord
testCodec =
    success TestRecord
        |> requiring "a" long .a
        |> requiring "b" string .b
        |> record { baseName = "test", nameSpace = [] }


hexBytes : List Int -> Bytes
hexBytes i =
    List.map Bytes.Encode.unsignedInt8 i
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode


stringExample : String
stringExample =
    "foo"


stringExampleBytes : Bytes
stringExampleBytes =
    hexBytes [ 0x06, 0x66, 0x6F, 0x6F ]


testRecord : TestRecord
testRecord =
    TestRecord 27 "foo"


testRecordBytes : Bytes
testRecordBytes =
    hexBytes [ 0x36, 0x06, 0x66, 0x6F, 0x6F ]


arrayExample : List Int
arrayExample =
    [ 3, 27 ]


arrayExampleBytes : Bytes
arrayExampleBytes =
    hexBytes [ 0x04, 0x06, 0x36, 0x00 ]


testExample : Codec a -> Bytes -> a -> Expect.Expectation
testExample codec bytes example =
    let
        decoder =
            Avro.makeDecoder codec codec.schema

        decoded =
            Result.toMaybe decoder
                |> Maybe.andThen
                    (\d ->
                        Decode.decode d bytes
                    )
    in
    Expect.equal decoded (Just <| example)


suite : Test
suite =
    describe "Binary examples from the specification conform"
        [ test "String Example" <|
            \_ -> testExample Codec.string stringExampleBytes stringExample
        , test "Record Example" <|
            \_ -> testExample testCodec testRecordBytes testRecord
        , test "Array Example" <|
            \_ -> testExample (Codec.array Codec.long) arrayExampleBytes arrayExample
        ]
