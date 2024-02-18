module JsonSpecs exposing (suite)

import Avro.Json.Schema exposing (decodeSchema, encodeSchema)
import Avro.Json.Value as Avro
import Avro.Name as Name exposing (TypeName)
import Avro.Schema as Schema exposing (Field, Schema, SortOrder(..))
import Avro.Value as Avro
import Bytes.Encode as Encode
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode exposing (Decoder, decodeValue)
import Json.Encode exposing (Value)
import Set
import Test exposing (..)


example1 : String
example1 =
    """
    {
      "type": "record",
      "name": "test",
      "fields" : [
        {"name": "a", "type": "long"},
        {"name": "b", "type": "string"}
      ]
    }
    """


example1Expected : Schema
example1Expected =
    Schema.Record
        { name = { baseName = "test", nameSpace = [] }
        , aliases = []
        , doc = Nothing
        , fields =
            [ { aliases = []
              , default = Nothing
              , doc = Nothing
              , name = "a"
              , order = Nothing
              , type_ = Schema.Long { logicalType = Nothing }
              }
            , { aliases = []
              , default = Nothing
              , doc = Nothing
              , name = "b"
              , order = Nothing
              , type_ = Schema.String { logicalType = Nothing }
              }
            ]
        }


example2 : String
example2 =
    """{"type": "enum", "name": "Foo", "symbols": ["A", "B", "C", "D"] }"""


example2Expected : Schema
example2Expected =
    Schema.Enum
        { name = { baseName = "Foo", nameSpace = [] }
        , aliases = []
        , doc = Nothing
        , symbols = [ "A", "B", "C", "D" ]
        , default = Nothing
        }


example3 : String
example3 =
    """{"type": "array", "items": "long"}"""


example3Expected : Schema
example3Expected =
    Schema.Array { items = Schema.Long { logicalType = Nothing } }


tripper : Decoder a -> (a -> Value) -> a -> Expect.Expectation
tripper decoder encoder example =
    let
        encoded =
            encoder example

        decoded =
            decodeValue decoder encoded
    in
    Expect.equal decoded (Ok <| example)


tripSchema : Schema -> Expect.Expectation
tripSchema =
    tripper decodeSchema encodeSchema


tripValueWithSchema : ( Schema, Avro.Value ) -> Expect.Expectation
tripValueWithSchema ( s, v ) =
    tripper (Avro.decodeValue s) (Avro.encodeValue s) v


testExample : String -> Schema -> Expect.Expectation
testExample example expected =
    let
        decoded =
            Json.Decode.decodeString decodeSchema example
    in
    Expect.equal decoded (Ok <| expected)


fuzzBaseName : Fuzzer String
fuzzBaseName =
    Fuzz.oneOfValues [ "foo", "bar", "baz" ]


fuzzName : Fuzzer TypeName
fuzzName =
    Fuzz.map2
        (\n ns -> TypeName n ns)
        fuzzBaseName
        (Fuzz.list fuzzBaseName)


fuzzField : Int -> Fuzzer Field
fuzzField i =
    Fuzz.map6
        Field
        fuzzBaseName
        (Fuzz.list fuzzBaseName)
        (Fuzz.constant Nothing)
        (Fuzz.maybe (Fuzz.oneOfValues [ Ascending, Descending, Ignore ]))
        (Fuzz.lazy (\_ -> fuzzSchema i))
        (Fuzz.constant Nothing)


dedupeSchemas : List Schema -> List Schema
dedupeSchemas =
    dedupeOn (\s -> (Name.canonicalName (Schema.typeName s)).baseName)


dedupeFields : List Field -> List Field
dedupeFields =
    dedupeOn (\s -> s.name)


dedupeOn : (a -> comparable) -> List a -> List a
dedupeOn f schemas =
    List.foldr
        (\value ( acc, seen ) ->
            let
                canon =
                    f value
            in
            if Set.member canon seen then
                ( acc, seen )

            else
                ( value :: acc, Set.insert canon seen )
        )
        ( [], Set.empty )
        schemas
        |> (\( a, _ ) -> a)


fuzzSchema : Int -> Fuzzer Schema
fuzzSchema i =
    let
        base =
            [ Fuzz.constant Schema.Null
            , Fuzz.constant Schema.Boolean
            , Fuzz.map (\lt -> Schema.Int { logicalType = lt }) (Fuzz.maybe Fuzz.string)
            , Fuzz.map (\lt -> Schema.Long { logicalType = lt }) (Fuzz.maybe Fuzz.string)
            , Fuzz.constant Schema.Float
            , Fuzz.constant Schema.Double
            , Fuzz.map (\lt -> Schema.Bytes { logicalType = lt }) (Fuzz.maybe Fuzz.string)
            , Fuzz.map (\lt -> Schema.String { logicalType = lt }) (Fuzz.maybe Fuzz.string)
            ]

        compound =
            [ Fuzz.map
                (\items -> Schema.Array { items = items })
                (Fuzz.lazy (\_ -> fuzzSchema (i - 1)))
            , Fuzz.map
                (\values -> Schema.Map { values = values })
                (Fuzz.lazy (\_ -> fuzzSchema (i - 1)))
            , Fuzz.map3
                (\name aliases fields -> Schema.Record { name = name, aliases = aliases, fields = dedupeFields fields, doc = Nothing })
                fuzzName
                (Fuzz.list fuzzName)
                (Fuzz.listOfLengthBetween 1 4 (Fuzz.lazy (\_ -> fuzzField (i - 1))))
            , Fuzz.listOfLengthBetween 1
                10
                (Fuzz.lazy (\_ -> fuzzSchema (i - 1)))
                |> Fuzz.map (\options -> Schema.Union { options = dedupeSchemas options })
            , Fuzz.map3
                (\name aliases symbols -> Schema.Enum { name = name, aliases = aliases, symbols = dedupeOn identity symbols, doc = Nothing, default = Nothing })
                fuzzName
                (Fuzz.list fuzzName)
                (Fuzz.listOfLengthBetween 1 10 Fuzz.string)
            , Fuzz.map3
                (\name aliases size -> Schema.Fixed { name = name, aliases = aliases, size = size, logicalType = Nothing })
                fuzzName
                (Fuzz.list fuzzName)
                Fuzz.int
            ]
    in
    Fuzz.oneOf
        (if i > 0 then
            List.append base compound

         else
            base
        )


fuzzValue : Schema -> Fuzzer Avro.Value
fuzzValue s =
    case s of
        Schema.Null ->
            Fuzz.constant Avro.Null

        Schema.Boolean ->
            Fuzz.bool
                |> Fuzz.map Avro.Boolean

        Schema.Int _ ->
            Fuzz.int
                |> Fuzz.map Avro.Int

        Schema.Long _ ->
            Fuzz.int
                |> Fuzz.map Avro.Long

        Schema.Float ->
            Fuzz.niceFloat
                |> Fuzz.map Avro.Float

        Schema.Double ->
            Fuzz.niceFloat
                |> Fuzz.map Avro.Double

        Schema.Bytes _ ->
            Fuzz.list (Fuzz.intRange 0 255)
                |> Fuzz.map (List.map Encode.unsignedInt8 >> Encode.sequence >> Encode.encode >> Avro.Bytes)

        Schema.String _ ->
            Fuzz.string
                |> Fuzz.map Avro.String

        Schema.Array info ->
            Fuzz.list (fuzzValue info.items)
                |> Fuzz.map Avro.Array

        Schema.Map info ->
            Fuzz.list (Fuzz.pair Fuzz.string (fuzzValue info.values))
                |> Fuzz.map (Dict.fromList >> Avro.Map)

        Schema.Record info ->
            Fuzz.traverse (\field -> fuzzValue field.type_) info.fields
                |> Fuzz.map Avro.Record

        Schema.Enum info ->
            Fuzz.intRange 0 (List.length info.symbols - 1)
                |> Fuzz.map Avro.Enum

        Schema.Union info ->
            Fuzz.oneOf <|
                List.indexedMap (\ix inner -> fuzzValue inner |> Fuzz.map (Avro.Union ix)) info.options

        Schema.Fixed info ->
            Fuzz.list (Fuzz.constant info.size)
                |> Fuzz.map (List.map Encode.unsignedInt8 >> Encode.sequence >> Encode.encode >> Avro.Fixed info.name)

        Schema.NamedType _ ->
            Fuzz.invalid "Can't generate name type"


fuzzSchemaAndValue : Fuzzer ( Schema, Avro.Value )
fuzzSchemaAndValue =
    fuzzSchema 2
        |> Fuzz.andThen (\s -> fuzzValue s |> Fuzz.map (\v -> ( s, v )))


suite : Test
suite =
    describe "Json encoding"
        [ test "Record example " <|
            \_ -> testExample example1 example1Expected
        , test "Enum example " <|
            \_ -> testExample example2 example2Expected
        , test "Array example " <|
            \_ -> testExample example3 example3Expected
        , fuzz (fuzzSchema 3) "Schema should roundtrip" <|
            tripSchema
        , fuzz fuzzSchemaAndValue "Values should roundtrip" <|
            tripValueWithSchema
        ]
