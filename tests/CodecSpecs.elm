module CodecSpecs exposing (..)

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
        [ test "Null codec" <|
            \u -> trip u Avro.Codec.null
        , fuzz Fuzz.bool "Boolean codec" <|
            \b -> trip b Avro.Codec.bool
        , fuzz Fuzz.int "Int codec" <|
            \b -> trip b Avro.Codec.int
        , fuzz Fuzz.int "Long codec" <|
            \b -> trip b Avro.Codec.long
        , fuzz Fuzz.niceFloat "Float codec" <|
            \b -> trip b Avro.Codec.float64
        , fuzz Fuzz.niceFloat "Double codec" <|
            \b -> trip b Avro.Codec.float64
        , fuzz Fuzz.string "String codec" <|
            \b -> trip b Avro.Codec.string
        , fuzz (Fuzz.list Fuzz.string) "Array of String codec" <|
            \b -> trip b (Avro.Codec.array Avro.Codec.string)
        , fuzz (Fuzz.list (Fuzz.list Fuzz.string)) "Array of Array of String codec" <|
            \b -> trip b (Avro.Codec.array (Avro.Codec.array Avro.Codec.string))
        , fuzz (Fuzz.list <| Fuzz.pair Fuzz.string Fuzz.string) "Map of String codec" <|
            \b -> trip (Dict.fromList b) (Avro.Codec.dict Avro.Codec.string)
        , test "Record Example 1" <|
            \_ -> trip fredericulio personCodec
        , test "Record Example 2" <|
            \_ -> trip juglidrio personCodec
        , test "Record Example 3" <|
            \_ -> tripVersions basicilio personCodec basicCodec
        ]
