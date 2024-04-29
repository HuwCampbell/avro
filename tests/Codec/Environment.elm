module Codec.Environment exposing (Account, Person, suite)

import Avro
import Avro.Codec exposing (..)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect
import Test exposing (..)


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
        |> requiring "accounts" (array (namedType accountCodec)) .accounts
        |> record { baseName = "person", nameSpace = [] }


fredericulio : Person
fredericulio =
    Person "Fredericulio" 34 (Just "rather not say") []


juglidrio : Person
juglidrio =
    Person "Juglidrio" 52 Nothing [ Account 4 Nothing, Account 10 (Just "Bankers") ]


trip : Codec a -> a -> Expect.Expectation
trip codec example =
    tripVersions codec codec example


tripVersions : Codec a -> Codec a -> a -> Expect.Expectation
tripVersions reader writer example =
    let
        environment =
            Avro.makeEnvironment
                [ ( accountCodec.schema, accountCodec.schema )
                ]

        decoder =
            environment
                |> Result.andThen
                    (\e -> Avro.makeDecoderInEnvironment e reader writer.schema)

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


suite : Test
suite =
    describe "Environment Handling"
        [ test "Should round trip when an embedded type is referenced by name" <|
            \_ -> trip personCodec fredericulio
        , test "Should round trip a more complex example." <|
            \_ -> trip personCodec juglidrio
        ]
