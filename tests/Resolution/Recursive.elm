module Resolution.Recursive exposing (..)

import Avro.Codec exposing (..)
import Resolution.Base exposing (compatible)
import Test exposing (..)


type LinkedList
    = LinkedList Int (Maybe LinkedList)


example : LinkedList
example =
    LinkedList 1 (Just <| LinkedList 2 Nothing)


linkedCodecRecursive : Codec LinkedList
linkedCodecRecursive =
    let
        codec rec =
            success LinkedList
                |> requiring "item" int (\(LinkedList a _) -> a)
                |> optional "rest" rec (\(LinkedList _ a) -> a)
    in
    codec
        |> recursiveRecord { baseName = "LinkedList", nameSpace = [] }


linkedCodecRecursiveV2 : Codec LinkedList
linkedCodecRecursiveV2 =
    let
        codec rec =
            success LinkedList
                |> requiring "item" int (\(LinkedList a _) -> a)
                |> optional "rest" rec (\(LinkedList _ a) -> a)
    in
    codec
        |> recursiveRecord { baseName = "LinkedListV2", nameSpace = [] }
        |> withAliases [ { baseName = "LinkedList", nameSpace = [] } ]


suite : Test
suite =
    describe "Resolving compatible recursive types with an alias"
        [ test "Fully matching schemas, different name" <|
            \_ -> compatible linkedCodecRecursiveV2 linkedCodecRecursive example example
        ]
