module NamesSpec exposing (suite)

import Avro.Json.Schema exposing (decodeSchema)
import Avro.Name exposing (TypeName)
import Avro.Schema as Schema exposing (Schema)
import Expect
import Json.Decode
import Test exposing (..)


namesExample : String
namesExample =
    """
    {
      "type": "record",
      "name": "Example",
      "doc": "A simple name (attribute) and no namespace attribute: use the null namespace  the fullname is 'Example'.",
      "fields": [
        {
          "name": "inheritNull",
          "type": {
            "type": "enum",
            "name": "Simple",
            "doc": "A simple name (attribute) and no namespace attribute: inherit the null namespace of the enclosing type 'Example'. The fullname is 'Simple'.",
            "symbols": ["a", "b"]
          }
        }, {
          "name": "explicitNamespace",
          "type": {
            "type": "fixed",
            "name": "Simple",
            "namespace": "explicit",
            "doc": "A simple name (attribute) and a namespace (attribute); the fullname is 'explicit.Simple' (this is a different type than of the 'inheritNull' field).",
            "size": 12
          }
        }, {
          "name": "fullName",
          "type": {
            "type": "record",
            "name": "a.full.Name",
            "namespace": "ignored",
            "doc": "A name attribute with a fullname, so the namespace attribute is ignored. The fullname is 'a.full.Name', and the namespace is 'a.full'.",
            "fields": [
              {
                "name": "inheritNamespace",
                "type": {
                  "type": "enum",
                  "name": "Understanding",
                  "doc": "A simple name (attribute) and no namespace attribute: inherit the namespace of the enclosing type 'a.full.Name'. The fullname is 'a.full.Understanding'.",
                  "symbols": ["d", "e"]
                }
              }
            ]
          }
        }
      ]
    }
    """


namesExpected : Schema
namesExpected =
    Schema.Record
        { name = { baseName = "Example", nameSpace = [] }
        , aliases = []
        , doc = Just "A simple name (attribute) and no namespace attribute: use the null namespace  the fullname is 'Example'."
        , fields =
            [ { aliases = []
              , default = Nothing
              , doc = Nothing
              , name = "inheritNull"
              , order = Nothing
              , type_ =
                    Schema.Enum
                        { aliases = []
                        , doc = Just "A simple name (attribute) and no namespace attribute: inherit the null namespace of the enclosing type 'Example'. The fullname is 'Simple'."
                        , default = Nothing
                        , name = TypeName "Simple" []
                        , symbols = [ "a", "b" ]
                        }
              }
            , { aliases = []
              , default = Nothing
              , doc = Nothing
              , name = "explicitNamespace"
              , order = Nothing
              , type_ =
                    Schema.Fixed
                        { aliases = []
                        , name = TypeName "Simple" [ "explicit" ]
                        , size = 12
                        , logicalType = Nothing
                        }
              }
            , { aliases = []
              , default = Nothing
              , doc = Nothing
              , name = "fullName"
              , order = Nothing
              , type_ =
                    Schema.Record
                        { aliases = []
                        , name = TypeName "Name" [ "a", "full" ]
                        , doc = Just "A name attribute with a fullname, so the namespace attribute is ignored. The fullname is 'a.full.Name', and the namespace is 'a.full'."
                        , fields =
                            [ { aliases = []
                              , default = Nothing
                              , doc = Nothing
                              , name = "inheritNamespace"
                              , order = Nothing
                              , type_ =
                                    Schema.Enum
                                        { aliases = []
                                        , doc = Just "A simple name (attribute) and no namespace attribute: inherit the namespace of the enclosing type 'a.full.Name'. The fullname is 'a.full.Understanding'."
                                        , default = Nothing
                                        , name = TypeName "Understanding" [ "a", "full" ]
                                        , symbols = [ "d", "e" ]
                                        }
                              }
                            ]
                        }
              }
            ]
        }


testExample : String -> Schema -> Expect.Expectation
testExample example expected =
    let
        decoded =
            Json.Decode.decodeString decodeSchema example
    in
    Expect.equal decoded (Ok <| expected)


suite : Test
suite =
    describe "Contextual Names"
        [ test "Specification Example" <|
            \_ -> testExample namesExample namesExpected
        ]
