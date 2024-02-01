module UnionSpecs exposing (..)

import Avro.Codec exposing (..)
import Avro.Internal.Deconflict as Deconflict
import Avro.Internal.Parser as Internal exposing (makeDecoder)
import Avro.Internal.ReadSchema as ReadSchema
import Avro.Schema as Schema
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Expect
import Fuzz
import Test exposing (..)


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


trip : Codec a -> a -> Expect.Expectation
trip codec example =
    tripVersions identity example codec codec


tripVersions : (a -> b) -> a -> Codec b -> Codec a -> Expect.Expectation
tripVersions inject example reader writer =
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
                        Decode.decode (makeDecoder Dict.empty p) encoded
                    )

        decoded =
            result
                |> Maybe.andThen reader.decoder
    in
    Expect.equal decoded (Just <| inject example)


readSchemaOf : Schema.Schema -> Maybe ReadSchema.ReadSchema
readSchemaOf s =
    Deconflict.deconflict s s


fuzzStudent : Fuzz.Fuzzer Student
fuzzStudent =
    Fuzz.map4
        Student
        Fuzz.string
        Fuzz.int
        (Fuzz.maybe (Fuzz.oneOfValues [ "m", "f" ]))
        (Fuzz.list <|
            Fuzz.map
                Degree
                Fuzz.string
        )


fuzzStaff : Fuzz.Fuzzer Staff
fuzzStaff =
    Fuzz.map2
        Staff
        Fuzz.string
        (Fuzz.maybe Fuzz.string)


fuzzAcademic : Fuzz.Fuzzer Academic
fuzzAcademic =
    Fuzz.map3
        Academic
        Fuzz.string
        Fuzz.string
        Fuzz.string


fuzzPeople : Fuzz.Fuzzer People
fuzzPeople =
    Fuzz.oneOf
        [ Fuzz.map Student_ fuzzStudent
        , Fuzz.map Staff_ fuzzStaff
        , Fuzz.map Academic_ fuzzAcademic
        ]


suite : Test
suite =
    describe "Union Codec tripping"
        [ describe "Written with complete codec"
            [ test "Staff Example" <|
                \_ -> trip peopleCodec (Staff_ { name = "Frank", school = Nothing })
            , test "Student Example" <|
                \_ -> trip peopleCodec (Student_ { name = "a", age = 1, sex = Nothing, degrees = [] })
            , test "Academic Example" <|
                \_ -> trip peopleCodec (Academic_ { name = "a", school = "QLC", title = "A.Prof" })
            ]
        , describe "Written with partial codec deconflicting"
            [ test "Staff Example" <|
                \_ -> tripVersions Staff_ { name = "Frank", school = Nothing } peopleCodec staffCodec
            , test "Student Example" <|
                \_ -> tripVersions Student_ { name = "a", age = 1, sex = Nothing, degrees = [] } peopleCodec studentCodec
            , test "Academic Example" <|
                \_ -> tripVersions Academic_ { name = "a", school = "QLC", title = "A.Prof" } peopleCodec academicCodec
            ]
        , describe "Round tripping"
            [ fuzz fuzzPeople "People should round trip" <|
                trip peopleCodec
            ]
        ]
