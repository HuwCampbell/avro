module Generators exposing (fuzzSchema, fuzzSchemaAndValue)

import Avro.Name exposing (TypeName)
import Avro.Schema as Schema exposing (Field, Schema, SortOrder(..))
import Avro.Value as Avro
import Avro.Value.Int64 as Int64
import Bytes.Encode as Encode
import Dict
import Fuzz exposing (Fuzzer)
import Generators.Corpus as Corpus
import Set


fuzzBaseName : Fuzzer String
fuzzBaseName =
    Corpus.boats


fuzzName : Fuzzer TypeName
fuzzName =
    Fuzz.map2
        (\n ns -> TypeName n ns)
        fuzzBaseName
        (Fuzz.list Corpus.weather)


fuzzDocs : Fuzzer (Maybe String)
fuzzDocs =
    Fuzz.maybe Corpus.glass


fuzzField : Int -> Fuzzer Field
fuzzField size =
    fuzzSchema size
        |> Fuzz.andThen
            (\schema ->
                Fuzz.map6
                    Field
                    Corpus.waters
                    (Fuzz.list Corpus.weather)
                    fuzzDocs
                    (Fuzz.maybe (Fuzz.oneOfValues [ Ascending, Descending, Ignore ]))
                    (Fuzz.constant schema)
                    (Fuzz.maybe (fuzzDefaultValue schema))
            )


dedupeSchemas : List Schema -> List Schema
dedupeSchemas =
    dedupeOn (\s -> (Schema.typeName s).baseName)


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


flattenUnions : List Schema -> List Schema
flattenUnions =
    let
        unionOptions s =
            case s of
                Schema.Union { options } ->
                    flattenUnions options

                other ->
                    [ other ]
    in
    List.concatMap unionOptions


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

        fuzzRec =
            Fuzz.lazy (\_ -> fuzzSchema (i - 1))

        compound =
            [ Fuzz.map
                (\items -> Schema.Array { items = items })
                fuzzRec
            , Fuzz.map
                (\values -> Schema.Map { values = values })
                fuzzRec
            , Fuzz.map4
                (\name aliases doc fields -> Schema.Record { name = name, aliases = aliases, fields = dedupeFields fields, doc = doc })
                fuzzName
                (Fuzz.list fuzzName)
                fuzzDocs
                (Fuzz.listOfLengthBetween 1 10 (Fuzz.lazy (\_ -> fuzzField (i - 1))))
            , Fuzz.listOfLengthBetween 1 10 fuzzRec
                |> Fuzz.map (\options -> Schema.Union { options = dedupeSchemas <| flattenUnions options })
            , Fuzz.map4
                (\name aliases doc symbols -> Schema.Enum { name = name, aliases = aliases, symbols = dedupeOn identity symbols, doc = doc, default = Nothing })
                fuzzName
                (Fuzz.list fuzzName)
                fuzzDocs
                (Fuzz.listOfLengthBetween 1 10 Fuzz.string)
            , Fuzz.map4
                (\name aliases doc size -> Schema.Fixed { name = name, aliases = aliases, size = size, logicalType = Nothing, doc = doc })
                fuzzName
                (Fuzz.list fuzzName)
                fuzzDocs
                (Fuzz.intRange 0 20)
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
            Fuzz.intRange (-2 ^ 52) (2 ^ 52)
                |> Fuzz.map (Avro.Long << Int64.fromInt)

        Schema.Float ->
            Fuzz.intRange 0 255
                |> Fuzz.map toFloat
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
            Fuzz.listOfLength info.size (Fuzz.intRange 0 255)
                |> Fuzz.map (List.map Encode.unsignedInt8 >> Encode.sequence >> Encode.encode >> Avro.Fixed)

        Schema.NamedType _ ->
            Fuzz.invalid "Can't generate name type"


fuzzDefaultValue : Schema -> Fuzzer Avro.Value
fuzzDefaultValue s =
    case s of
        Schema.Union { options } ->
            case options of
                x :: _ ->
                    Fuzz.map (Avro.Union 0) <|
                        fuzzValue x

                _ ->
                    Fuzz.invalid "Can't generated default value for empty union"

        _ ->
            fuzzValue s


fuzzSchemaAndValue : Fuzzer ( Schema, Avro.Value )
fuzzSchemaAndValue =
    fuzzSchema 2
        |> Fuzz.andThen (\s -> fuzzValue s |> Fuzz.map (\v -> ( s, v )))
