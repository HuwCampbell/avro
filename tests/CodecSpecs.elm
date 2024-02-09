module CodecSpecs exposing (..)

import Avro.Codec exposing (..)
import Avro.Internal.Bytes as Internal exposing (makeDecoder)
import Avro.Internal.Deconflict as Deconflict
import Avro.Internal.ReadSchema as ReadSchema
import Avro.Schema as Schema
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Expect
import Fuzz
import Test exposing (..)


encodeBytes : List Int -> Bytes
encodeBytes bs =
    List.map (\b -> Encode.unsignedInt8 b) bs
        |> Encode.sequence
        |> Encode.encode


type alias Person =
    { who : String, age : Int, sex : Maybe String, accounts : List Account }


type alias Account =
    { id : Int, kind : Maybe String }


accountCodec : Codec Account
accountCodec =
    success Account
        |> requiring "id" int .id
        |> optional "kind" string .kind
        |> record { baseName = "account", nameSpace = [] }


personCodec : Codec Person
personCodec =
    success Person
        |> requiring "who" string .who
        |> requiring "age" int .age
        |> optional "sex" string .sex
        |> withFallback "accounts" (array accountCodec) [] .accounts
        |> record { baseName = "person", nameSpace = [] }


basicCodec : Codec Person
basicCodec =
    success (\age who -> { age = age, who = who, sex = Nothing, accounts = [] })
        |> requiring "age" int .age
        |> requiring "who" string .who
        |> record { baseName = "person", nameSpace = [] }


example2 =
    map2 Person
        (structField "name" [] Nothing Nothing string Nothing |> lmap .name)
        (structField "age" [] Nothing Nothing int Nothing |> lmap .age)


basicilio =
    Person "Basicilio" 84 Nothing []


fredericulio =
    Person "Fredericulio" 34 (Just "rather not say") []


juglidrio =
    Person "Juglidrio" 52 Nothing [ Account 4 Nothing, Account 10 (Just "Bankers") ]


trip : Codec a -> a -> Expect.Expectation
trip codec example =
    tripVersions codec codec example


tripVersions : Codec a -> Codec a -> a -> Expect.Expectation
tripVersions reader writer example =
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
    Expect.equal decoded (Just <| example)


readSchemaOf : Schema.Schema -> Maybe ReadSchema.ReadSchema
readSchemaOf s =
    Deconflict.deconflict s s


suite : Test
suite =
    describe "The Codecs Module"
        [ test "Round trip null codec" <|
            trip Avro.Codec.null
        , fuzz Fuzz.bool "Round trip boolean codec" <|
            trip Avro.Codec.bool
        , fuzz Fuzz.int "Round trip int codec" <|
            trip Avro.Codec.int
        , fuzz Fuzz.int "Round trip long codec" <|
            trip Avro.Codec.long
        , fuzz Fuzz.niceFloat "Round trip float codec" <|
            trip Avro.Codec.float64
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
        , test "Should round trip a simple record example." <|
            \_ -> trip personCodec fredericulio
        , test "Should round trip a more complex example." <|
            \_ -> trip personCodec juglidrio
        , test "Should round trip data written with a compatible codec with defaulted fields" <|
            \_ -> tripVersions personCodec basicCodec basicilio
        ]
