module Avro.Json.Schema exposing (decodeSchema, encodeSchema)

import Avro.Json.Value exposing (decodeDefaultValue, encodeDefaultValue)
import Avro.Name exposing (TypeName, contextualTypeName, parseTypeName)
import Avro.Schema exposing (Field, Schema(..), SortOrder(..))
import Json.Decode as Decode exposing (Decoder, oneOf)
import Json.Encode as Encode exposing (Value)
import Maybe exposing (withDefault)


encodeSchema : Schema -> Value
encodeSchema s =
    case s of
        Null ->
            Encode.string "null"

        Boolean ->
            Encode.string "boolean"

        Int { logicalType } ->
            case logicalType of
                Nothing ->
                    Encode.string "int"

                Just lt ->
                    Encode.object
                        [ ( "type", Encode.string "int" )
                        , ( "logicalType", Encode.string lt )
                        ]

        Long { logicalType } ->
            case logicalType of
                Nothing ->
                    Encode.string "long"

                Just lt ->
                    Encode.object
                        [ ( "type", Encode.string "long" )
                        , ( "logicalType", Encode.string lt )
                        ]

        Float ->
            Encode.string "float"

        Double ->
            Encode.string "double"

        Bytes ->
            Encode.string "bytes"

        String { logicalType } ->
            case logicalType of
                Nothing ->
                    Encode.string "string"

                Just lt ->
                    Encode.object
                        [ ( "type", Encode.string "string" )
                        , ( "logicalType", Encode.string lt )
                        ]

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
                    let
                        required =
                            [ ( "name", Encode.string f.name )
                            , ( "aliases", Encode.list Encode.string f.aliases )
                            , ( "type", encodeSchema f.type_ )
                            ]

                        optionals =
                            [ ( "doc", Maybe.map Encode.string f.doc )
                            , ( "order", Maybe.map encodeSortOrder f.order )
                            , ( "default", Maybe.map (encodeDefaultValue f.type_) f.default )
                            ]
                    in
                    Encode.object <|
                        required
                            ++ encodeOptionals optionals
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
        contextualTypeName
        (Decode.field "name" Decode.string)
        (optionalField "namespace" Decode.string)
        |> Decode.andThen
            (\name ->
                case name of
                    Just nm ->
                        Decode.succeed nm

                    Nothing ->
                        Decode.fail "Could not parse type name"
            )


decodeAliases : Decoder (List TypeName)
decodeAliases =
    Decode.map
        (\aliases -> List.map (\n -> TypeName n []) (Maybe.withDefault [] aliases))
        (optionalField "aliases" (Decode.list Decode.string))


optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField field decoder =
    let
        -- This hack is from the decoder pipeline package.
        -- The idea is that you parse a value, then reparse it.
        -- If there's a failure it's missing, but you can give
        -- a good error message if it fails because the field is
        -- wrong.
        nullOr =
            Decode.oneOf [ decoder |> Decode.map Just, Decode.null Nothing ]

        handleResult input =
            case Decode.decodeValue (Decode.at [ field ] Decode.value) input of
                Ok rawValue ->
                    case Decode.decodeValue nullOr rawValue of
                        Ok finalResult ->
                            Decode.succeed finalResult

                        Err _ ->
                            Decode.at [ field ] nullOr

                Err _ ->
                    Decode.succeed Nothing
    in
    Decode.value
        |> Decode.andThen handleResult


encodeSortOrder : SortOrder -> Value
encodeSortOrder order =
    case order of
        Ascending ->
            Encode.string "ascending"

        Descending ->
            Encode.string "descending"

        Ignore ->
            Encode.string "ignore"


decodeSortOrder : Decoder SortOrder
decodeSortOrder =
    Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "ascending" ->
                        Decode.succeed Ascending

                    "descending" ->
                        Decode.succeed Descending

                    "ignore" ->
                        Decode.succeed Ignore

                    _ ->
                        Decode.fail "Not a valid Sort order"
            )


decodeFields : Decoder Field
decodeFields =
    Decode.map6
        Field
        (Decode.field "name" Decode.string)
        (optionalField "aliases" (Decode.list Decode.string) |> Decode.map (withDefault []))
        (optionalField "doc" Decode.string)
        (optionalField "order" decodeSortOrder)
        (Decode.field "type" decodeSchema)
        (Decode.field "type" decodeSchema |> Decode.andThen (optionalField "default" << decodeDefaultValue))


decodeSchema : Decoder Schema
decodeSchema =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\tag ->
                    case tag of
                        "null" ->
                            Decode.succeed Null

                        "boolean" ->
                            Decode.succeed Boolean

                        "int" ->
                            Decode.succeed (Int { logicalType = Nothing })

                        "long" ->
                            Decode.succeed (Long { logicalType = Nothing })

                        "float" ->
                            Decode.succeed Float

                        "double" ->
                            Decode.succeed Double

                        "bytes" ->
                            Decode.succeed Bytes

                        "string" ->
                            Decode.succeed (String { logicalType = Nothing })

                        _ ->
                            Decode.fail "Not a primitive type"
                )
        , Decode.string
            |> Decode.andThen
                (\s ->
                    case parseTypeName s of
                        Just nt ->
                            Decode.succeed (NamedType nt)

                        Nothing ->
                            Decode.fail "Can't parse as named type"
                )
        , Decode.field "type" Decode.string
            |> Decode.andThen
                (\tag ->
                    case tag of
                        "null" ->
                            Decode.succeed Null

                        "boolean" ->
                            Decode.succeed Boolean

                        "int" ->
                            Decode.map
                                (\logicalType -> Int { logicalType = logicalType })
                                (optionalField "logicalType" Decode.string)

                        "long" ->
                            Decode.map
                                (\logicalType -> Long { logicalType = logicalType })
                                (optionalField "logicalType" Decode.string)

                        "float" ->
                            Decode.succeed Float

                        "double" ->
                            Decode.succeed Double

                        "bytes" ->
                            Decode.succeed Bytes

                        "string" ->
                            Decode.map
                                (\logicalType -> String { logicalType = logicalType })
                                (optionalField "logicalType" Decode.string)

                        "array" ->
                            Decode.map
                                (\items -> Array { items = items })
                                (Decode.field "items" (Decode.lazy (\_ -> decodeSchema)))

                        "map" ->
                            Decode.map
                                (\values -> Map { values = values })
                                (Decode.field "values" (Decode.lazy (\_ -> decodeSchema)))

                        "record" ->
                            Decode.map4
                                (\name aliases doc fields -> Record { name = name, aliases = aliases, fields = fields, doc = doc })
                                decodeName
                                decodeAliases
                                (optionalField "doc" Decode.string)
                                (Decode.field "fields" (Decode.list decodeFields))

                        "fixed" ->
                            Decode.map3
                                (\name aliases size -> Fixed { name = name, aliases = aliases, size = size })
                                decodeName
                                decodeAliases
                                (Decode.field "size" Decode.int)

                        "enum" ->
                            Decode.map5
                                (\name aliases doc symbols default -> Enum { name = name, aliases = aliases, symbols = symbols, doc = doc, default = default })
                                decodeName
                                decodeAliases
                                (optionalField "doc" Decode.string)
                                (Decode.field "symbols" (Decode.list Decode.string))
                                (optionalField "default" Decode.string)

                        _ ->
                            Decode.fail "Not a primitive type"
                )
        , Decode.map
            (\options -> Union { options = options })
            (Decode.list (Decode.lazy (\_ -> decodeSchema)))
        ]


encodeOptionals : List ( String, Maybe Value ) -> List ( String, Value )
encodeOptionals =
    List.filterMap (\( v, s ) -> Maybe.map (\ss -> ( v, ss )) s)
