module Codec.Union exposing (Academic, Degree, People(..), Staff, Student, suite)

import Avro
import Avro.Codec exposing (..)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
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


twoOfThree : Codec (Result Student Staff)
twoOfThree =
    union studentCodec staffCodec


injectTwoOfThree : Result Student Staff -> People
injectTwoOfThree results =
    case results of
        Err student ->
            Student_ student

        Ok staff ->
            Staff_ staff


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
    describe "Union Codecs should round trip"
        [ describe "Union of 3"
            [ fuzz fuzzPeople "People should round trip" <|
                trip peopleCodec
            ]
        , describe "Written with single item codec and deconflicting"
            [ fuzz fuzzStaff "Staff" <|
                tripVersions Staff_ peopleCodec staffCodec
            , fuzz fuzzStudent "Student Example" <|
                tripVersions Student_ peopleCodec studentCodec
            , fuzz fuzzAcademic "Academic Example" <|
                tripVersions Academic_ peopleCodec academicCodec
            ]
        , describe "Written with union codec missing some"
            [ fuzz fuzzStaff "Staff" <|
                tripVersions injectTwoOfThree peopleCodec twoOfThree
                    << Ok
            , fuzz fuzzStudent "Student" <|
                tripVersions injectTwoOfThree peopleCodec twoOfThree
                    << Err
            ]
        ]
