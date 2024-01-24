module UnionSpecs exposing (..)

import Avro.Codec exposing (..)
import Avro.Internal.Deconflict as Deconflict
import Avro.Internal.Parser as Internal exposing (makeDecoder)
import Avro.Internal.ReadSchema as ReadSchema
import Avro.Schema as Schema
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Test exposing (..)


encodeBytes : List Int -> Bytes
encodeBytes bs =
    List.map (\b -> Encode.unsignedInt8 b) bs
        |> Encode.sequence
        |> Encode.encode


type alias Degree =
    { name : String }


type alias Student =
    { name : String, age : Int, sex : Maybe String, degrees : List Degree }


type alias Staff =
    { name : String, school : Maybe String }


type alias Academic =
    { name : String, school : String, title : String }


type People
    = Student_ Student
    | Staff_ Staff
    | Academic_ Academic


degreeCodec : Codec Degree
degreeCodec =
    success Degree
        |> requiring "name" string .name
        |> record { baseName = "degree", nameSpace = [] }


studentCodec : Codec Student
studentCodec =
    success Student
        |> requiring "name" string .name
        |> requiring "age" int .age
        |> optional "sex" string .sex
        |> withFallback "degrees" (array degreeCodec) [] .degrees
        |> record { baseName = "student", nameSpace = [] }


staffCodec : Codec Staff
staffCodec =
    success Staff
        |> requiring "name" string .name
        |> optional "school" string .school
        |> record { baseName = "staff", nameSpace = [] }


academicCodec : Codec Academic
academicCodec =
    success Academic
        |> requiring "name" string .name
        |> requiring "school" string .school
        |> requiring "title" string .title
        |> record { baseName = "academic", nameSpace = [] }


peopleCodec : Codec People
peopleCodec =
    let
        from results =
            case results of
                Err student ->
                    Student_ student

                Ok (Err staff) ->
                    Staff_ staff

                Ok (Ok academic) ->
                    Academic_ academic

        to results =
            case results of
                Student_ student ->
                    Err student

                Staff_ staff ->
                    Ok (Err staff)

                Academic_ academic ->
                    Ok (Ok academic)
    in
    imap to from <|
        union3 studentCodec staffCodec academicCodec


trip : a -> Codec a -> Expect.Expectation
trip example codec =
    tripVersions example codec codec


tripVersions : a -> Codec a -> Codec a -> Expect.Expectation
tripVersions example reader writer =
    let
        decoflicted =
            Deconflict.deconflict reader.schema writer.schema

        encoded =
            writer.writer example
                |> Internal.encodeValue
                |> Encode.encode

        result =
            decoflicted
                |> Maybe.andThen
                    (\p ->
                        Decode.decode (makeDecoder p) encoded
                    )

        decoded =
            result
                |> Maybe.andThen reader.decoder
    in
    Expect.equal decoded (Just <| example)


readSchemaOf : Schema.Schema -> Maybe ReadSchema.ReadSchema
readSchemaOf s =
    Deconflict.deconflict s s


suite : Test
suite =
    describe "Union funtimes"
        [ test "Example 1" <|
            \_ -> trip (Staff_ { name = "Frank", school = Nothing }) peopleCodec
        ]
