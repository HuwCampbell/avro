module Resolution.Basics exposing (suite)

import Avro.Codec as Codec exposing (Codec)
import Fuzz
import Resolution.Base exposing (compatible)
import Test exposing (..)


version1 : Codec { name : String }
version1 =
    Codec.success (\n -> { name = n })
        |> Codec.requiring "name" Codec.string .name
        |> Codec.record { baseName = "Person", nameSpace = [] }


version2 : Codec { name : String, age : Maybe Int }
version2 =
    Codec.success (\n a -> { name = n, age = a })
        |> Codec.requiring "name" Codec.string .name
        |> Codec.optional "age" Codec.int .age
        |> Codec.record { baseName = "PersonV2", nameSpace = [] }
        |> Codec.withAliases [ { baseName = "Person", nameSpace = [] } ]


largeNumber : number
largeNumber =
    914793674309632


suite : Test
suite =
    describe "Schema deconflicting module"
        [ describe "Basic types and unions"
            [ fuzz Fuzz.int "Int to Long" <|
                \i -> compatible Codec.long Codec.int i i
            , fuzz Fuzz.int "Int to Float" <|
                \i -> compatible Codec.float32 Codec.int (Basics.toFloat i) i
            , fuzz Fuzz.int "Long to Float" <|
                \i -> compatible Codec.float32 Codec.long (Basics.toFloat i) i
            , fuzz Fuzz.int "Int to Double" <|
                \i -> compatible Codec.float64 Codec.int (Basics.toFloat i) i
            , fuzz Fuzz.int "Long to Double" <|
                \i -> compatible Codec.float64 Codec.long (Basics.toFloat i) i
            , test "Larger Long to Float" <|
                \_ -> compatible Codec.float32 Codec.long largeNumber largeNumber
            , test "Larger Long to Double" <|
                \_ -> compatible Codec.float64 Codec.long largeNumber largeNumber
            ]
        , describe "Records"
            [ test "Record with new name (and alias to old) and new optional field" <|
                \_ -> compatible version2 version1 { name = "Francis", age = Nothing } { name = "Francis" }
            ]
        ]
