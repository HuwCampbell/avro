module Codec.Basics exposing (Account, Person, suite)

import Avro
import Avro.Codec exposing (..)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Expect
import Fuzz
import Test exposing (..)


type alias Person =
    { who : String, age : Int, sex : Maybe String, accounts : List Account }


type alias Account_ a =
    { id : Int, kind : Maybe String, cosignatories : List a }


type Account
    = Account (Account_ Person)


accountCodecBuilder : Codec Person -> Codec Account
accountCodecBuilder rec =
    success Account_
        |> requiring "id" int .id
        |> optional "kind" string .kind
        |> withFallback "cosignatories" (array rec) [] .cosignatories
        |> dimap (\(Account a) -> a) Account
        |> record { baseName = "account", nameSpace = [] }


personCodec : Codec Person
personCodec =
    let
        builder rec =
            success Person
                |> requiring "who" string .who
                |> requiring "age" int .age
                |> optional "sex" string .sex
                |> withFallback "accounts" (array (namedType (accountCodecBuilder rec))) [] .accounts
    in
    builder
        |> recursiveRecord { baseName = "person", nameSpace = [] }


accountCodec : Codec Account
accountCodec =
    accountCodecBuilder (namedType personCodec)


basicCodec : Codec Person
basicCodec =
    success (\age who -> { age = age, who = who, sex = Nothing, accounts = [] })
        |> requiring "age" int .age
        |> requiring "who" string .who
        |> record { baseName = "person", nameSpace = [] }


type Colour
    = Red
    | Blue
    | Green


colourCodec : Codec Colour
colourCodec =
    let
        renderColour c =
            case c of
                Red ->
                    0

                Blue ->
                    1

                Green ->
                    2

        parseColour ix =
            case ix of
                0 ->
                    Just Red

                1 ->
                    Just Blue

                2 ->
                    Just Green

                _ ->
                    Nothing
    in
    enum
        { baseName = "colour", nameSpace = [] }
        [ "red", "blue", "green" ]
        Nothing
        |> emap renderColour parseColour


basicilio : Person
basicilio =
    Person "Basicilio" 84 Nothing []


fredericulio : Person
fredericulio =
    Person "Fredericulio" 34 (Just "rather not say") []


juglidrio : Person
juglidrio =
    Person "Juglidrio" 52 Nothing [ Account <| Account_ 4 Nothing [ basicilio ], Account <| Account_ 10 (Just "Bankers") [ fredericulio ] ]


business : Account
business =
    Account <| Account_ 4 Nothing [ juglidrio ]


trip : Codec a -> a -> Expect.Expectation
trip codec example =
    tripVersions codec codec example


tripVersions : Codec a -> Codec a -> a -> Expect.Expectation
tripVersions reader writer example =
    let
        env =
            { readerEnvironment = [ accountCodec.schema, personCodec.schema ]
            , writerEnvironment = [ accountCodec.schema, personCodec.schema ]
            }

        decoder =
            Avro.makeDecoderInEnvironment env reader writer.schema

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


largerInt : Fuzz.Fuzzer Int
largerInt =
    Fuzz.intRange (-2 ^ 52) (2 ^ 52)


suite : Test
suite =
    describe "The Codecs Module"
        [ test "Round trip null codec" <|
            trip Avro.Codec.null
        , fuzz Fuzz.bool "Round trip boolean codec" <|
            trip Avro.Codec.bool
        , fuzz Fuzz.int "Round trip int codec" <|
            trip Avro.Codec.int
        , fuzz largerInt "Round trip long codec" <|
            trip Avro.Codec.long
        , fuzz Fuzz.niceFloat "Round trip double codec" <|
            trip Avro.Codec.float64
        , fuzz Fuzz.string "Round trip string codec" <|
            trip Avro.Codec.string
        , fuzz (Fuzz.list Fuzz.string) "Round trip Array of String codec" <|
            trip (Avro.Codec.array Avro.Codec.string)
        , fuzz (Fuzz.list (Fuzz.list Fuzz.string)) "Round trip Array of Array of String codec" <|
            trip (Avro.Codec.array (Avro.Codec.array Avro.Codec.string))
        , fuzz (Fuzz.list <| Fuzz.pair Fuzz.string Fuzz.string) "Round trip Map of String codec" <|
            trip (Avro.Codec.dict Avro.Codec.string)
                << Dict.fromList
        , fuzz (Fuzz.oneOfValues [ Red, Blue, Green ]) "Round trip Colour Enum codec" <|
            trip colourCodec
        , test "Should round trip a simple record example." <|
            \_ -> trip personCodec fredericulio
        , test "Should round trip a more complex example." <|
            \_ -> trip personCodec juglidrio
        , test "Should round trip with the alternate base" <|
            \_ -> trip accountCodec business
        , test "Should round trip data written with a compatible codec with defaulted fields" <|
            \_ -> tripVersions personCodec basicCodec basicilio
        ]
