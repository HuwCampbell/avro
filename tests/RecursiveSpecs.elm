module RecursiveSpecs exposing (..)

import Avro.Codec exposing (..)
import Avro.Deconflict as Deconflict
import Avro.Internal.Bytes as Internal exposing (makeDecoder)
import Avro.ReadSchema as ReadSchema
import Avro.Schema as Schema
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Expect
import Fuzz
import Test exposing (..)


type LinkedList
    = LinkedList Int (Maybe LinkedList)


linkedCodecRecursive : Codec LinkedList
linkedCodecRecursive =
    let
        codec rec =
            success LinkedList
                |> requiring "item" int (\(LinkedList a _) -> a)
                |> optional "rest" rec (\(LinkedList _ a) -> a)
                |> record { baseName = "LinkedList", nameSpace = [] }
    in
    recursive codec


linkedCodecRecursiveRecord : Codec (List Int)
linkedCodecRecursiveRecord =
    recursiveRecord { baseName = "list", nameSpace = [] }
        (\rec ->
            let
                cons =
                    success (\a b -> ( a, b ))
                        |> requiring "head" long (\( a, b ) -> a)
                        |> requiring "tail" rec (\( a, b ) -> b)
                        |> record { baseName = "cons", nameSpace = [] }

                uncons x =
                    case x of
                        [] ->
                            Nothing

                        a :: aa ->
                            Just ( a, aa )

                recons x =
                    case x of
                        Nothing ->
                            []

                        Just ( a, aa ) ->
                            a :: aa
            in
            structField "box" [] Nothing Nothing (imap uncons recons (maybe cons)) (Just [])
        )


fuzzLinked : Fuzz.Fuzzer LinkedList
fuzzLinked =
    Fuzz.map2
        (\i is -> List.foldr (\a b -> LinkedList a (Just b)) (LinkedList i Nothing) is)
        Fuzz.int
        (Fuzz.list Fuzz.int)


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


suite : Test
suite =
    describe "Recursive Codec tripping"
        [ describe "Custom linked list"
            [ test "one value" <|
                \_ -> trip linkedCodecRecursive (LinkedList 1 Nothing)
            , test "two values" <|
                \_ -> trip linkedCodecRecursive (LinkedList 1 (Just (LinkedList 2 Nothing)))
            , fuzz fuzzLinked "any number" <|
                trip linkedCodecRecursive
            ]
        , describe "Standard linked list"
            [ test "no values" <|
                \_ -> trip linkedCodecRecursiveRecord []
            , test "one value" <|
                \_ -> trip linkedCodecRecursiveRecord [ 1 ]
            , test "two values" <|
                \_ -> trip linkedCodecRecursiveRecord [ 1, 2 ]
            , fuzz (Fuzz.list Fuzz.int) "any number" <|
                trip linkedCodecRecursiveRecord
            ]
        ]
