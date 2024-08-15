module Schema.Validation exposing (suite)

import Avro.Name exposing (TypeName)
import Avro.Schema as Schema exposing (Schema, SortOrder(..))
import Expect
import Generators
import Test exposing (..)


nestedUnion : Schema
nestedUnion =
    Schema.Union
        { options =
            [ Schema.Union { options = [] } ]
        }


duplicateUnion : Schema
duplicateUnion =
    Schema.Union
        { options =
            [ Schema.Double, Schema.Double ]
        }


duplicateEnum : Schema
duplicateEnum =
    Schema.Enum
        { name = TypeName "good" []
        , aliases = []
        , doc = Nothing
        , symbols = [ "a", "a" ]
        , default = Nothing
        }


badDefaultEnum : Schema
badDefaultEnum =
    Schema.Enum
        { name = TypeName "good" []
        , aliases = []
        , doc = Nothing
        , symbols = [ "a", "b" ]
        , default = Just "c"
        }


badNameEnum : Schema
badNameEnum =
    Schema.Enum
        { name = TypeName "go#@!od" []
        , aliases = []
        , doc = Nothing
        , symbols = [ "a", "b" ]
        , default = Just "a"
        }


shouldFail : Schema -> Expect.Expectation
shouldFail schema =
    Schema.validateSchema schema
        |> Expect.err


suite : Test
suite =
    describe "Schema Validation"
        [ test "Nested Union should fail validation" <|
            \_ -> shouldFail nestedUnion
        , test "Duplicates in union should fail validation" <|
            \_ -> shouldFail duplicateUnion
        , test "Duplicates in enum symbols should fail validation" <|
            \_ -> shouldFail duplicateEnum
        , test "Enum with bad default should fail validation" <|
            \_ -> shouldFail badDefaultEnum
        , test "Schema with bad name should fail validation" <|
            \_ -> shouldFail badNameEnum
        , fuzz (Generators.fuzzSchema 3) "Generated schemas should pass validation" <|
            Schema.validateSchema
                >> Expect.ok
        ]
