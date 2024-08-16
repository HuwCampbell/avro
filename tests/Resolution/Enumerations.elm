module Resolution.Enumerations exposing (..)

import Avro.Codec as Codec exposing (Codec)
import Avro.Name exposing (TypeName)
import Resolution.Base exposing (compatible)
import Test exposing (..)


version1 : Codec Int
version1 =
    Codec.enum (TypeName "myEnum" [ "n1" ]) [ "e1", "e3", "e4" ] Nothing


version2 : Codec Int
version2 =
    Codec.enum (TypeName "myEnum" [ "n1" ]) [ "e1", "e2", "e3" ] (Just "e2")


suite : Test
suite =
    describe "Enum defaulting module"
        [ describe "Resolving compatible enumerations with a default"
            [ test "Fully matching value (e1)" <|
                \_ -> compatible version2 version1 0 0
            , test "Shifted matching value (e3)" <|
                \_ -> compatible version2 version1 1 2
            , test "Removed value goes to default (e4 to e2)" <|
                \_ -> compatible version2 version1 2 1
            ]
        ]
