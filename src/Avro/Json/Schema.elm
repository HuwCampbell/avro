module Avro.Json.Schema exposing (..)

import Avro.Name exposing (TypeName)
import Avro.Schema exposing (Field, Schema(..))
import Json.Decode as Decode exposing (Decoder, oneOf)
import Json.Encode as Encode exposing (Value)
import Maybe exposing (withDefault)


encodeOrNull : (a -> Value) -> Maybe a -> Value
encodeOrNull f ma =
    case ma of
        Just a ->
            f a

        Nothing ->
            Encode.null


encodeSchema : Schema -> Value
encodeSchema s =
    case s of
        Null ->
            Encode.string "null"

        Boolean ->
            Encode.string "boolean"

        Int ->
            Encode.string "int"

        Long ->
            Encode.string "long"

        Float ->
            Encode.string "float"

        Double ->
            Encode.string "double"

        Bytes ->
            Encode.string "bytes"

        String ->
            Encode.string "string"

        Array info ->
            Encode.object
                [ ( "type", Encode.string "array" )
                , ( "items", encodeSchema info.items )
                ]

        Map info ->
            Encode.object
                [ ( "type", Encode.string "map" )
                , ( "values", encodeSchema info.values )
                ]

        Union info ->
            info.options
                |> Encode.list encodeSchema

        NamedType nm ->
            Encode.string nm.baseName

        Record info ->
            let
                encodeField f =
                    Encode.object
                        [ ( "name", Encode.string f.name )
                        , ( "type", encodeSchema f.type_ )
                        , ( "aliases", Encode.list Encode.string f.aliases )
                        , ( "doc", encodeOrNull Encode.string f.doc )
                        ]
            in
            Encode.object
                [ ( "type", Encode.string "record" )
                , ( "name", Encode.string info.name.baseName )
                , ( "aliases", Encode.list (Encode.string << .baseName) info.aliases )
                , ( "fields", Encode.list encodeField info.fields )
                ]

        Fixed info ->
            Encode.object
                [ ( "type", Encode.string "fixed" )
                , ( "name", Encode.string info.name.baseName )
                , ( "aliases", Encode.list (Encode.string << .baseName) info.aliases )
                , ( "size", Encode.int info.size )
                ]

        Enum info ->
            Encode.object
                [ ( "type", Encode.string "enum" )
                , ( "name", Encode.string info.name.baseName )
                , ( "aliases", Encode.list (Encode.string << .baseName) info.aliases )
                , ( "symbols", Encode.list Encode.string info.symbols )
                ]


decodeName : Decoder TypeName
decodeName =
    Decode.map2
        (\n _ -> TypeName n [])
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.field "namespace" Decode.string))


decodeAliases : Decoder (List TypeName)
decodeAliases =
    Decode.map
        (\aliases -> List.map (\n -> TypeName n []) (Maybe.withDefault [] aliases))
        (Decode.maybe (Decode.field "aliases" (Decode.list Decode.string)))


decodeFields : Decoder Field
decodeFields =
    Decode.map6
        Field
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.field "aliases" (Decode.list Decode.string)) |> Decode.map (withDefault []))
        (Decode.maybe (Decode.field "doc" Decode.string))
        (Decode.succeed Nothing)
        (Decode.field "type" decodeSchema)
        (Decode.succeed Nothing)


decodeSchema : Decoder Schema
decodeSchema =
    let
        decodeBaseType tag =
            case tag of
                "null" ->
                    Decode.succeed Null

                "boolean" ->
                    Decode.succeed Boolean

                "int" ->
                    Decode.succeed Int

                "long" ->
                    Decode.succeed Long

                "float" ->
                    Decode.succeed Float

                "double" ->
                    Decode.succeed Double

                "bytes" ->
                    Decode.succeed Bytes

                "string" ->
                    Decode.succeed String

                _ ->
                    Decode.fail "Not a known type"

        decodeComplexType tag =
            case tag of
                "array" ->
                    Decode.map
                        (\items -> Array { items = items })
                        (Decode.field "items" (Decode.lazy (\_ -> decodeSchema)))

                "map" ->
                    Decode.map
                        (\values -> Map { values = values })
                        (Decode.field "values" (Decode.lazy (\_ -> decodeSchema)))

                "record" ->
                    Decode.map3
                        (\name alias_ fields -> Record { name = name, aliases = alias_, fields = fields, doc = Nothing })
                        decodeName
                        decodeAliases
                        (Decode.field "fields" (Decode.list decodeFields))

                "fixed" ->
                    Decode.map3
                        (\name alias_ size -> Fixed { name = name, aliases = alias_, size = size })
                        decodeName
                        decodeAliases
                        (Decode.field "size" Decode.int)

                "enum" ->
                    Decode.map3
                        (\name alias_ symbols -> Enum { name = name, aliases = alias_, symbols = symbols, doc = Nothing })
                        decodeName
                        decodeAliases
                        (Decode.field "symbols" (Decode.list Decode.string))

                _ ->
                    Decode.fail "Not a known type"
    in
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                decodeBaseType
        , Decode.field "type" Decode.string
            |> Decode.andThen
                (\tag ->
                    oneOf
                        [ decodeBaseType tag
                        , decodeComplexType tag
                        ]
                )
        , Decode.map
            (\options -> Union { options = options })
            (Decode.list (Decode.lazy (\_ -> decodeSchema)))
        ]
