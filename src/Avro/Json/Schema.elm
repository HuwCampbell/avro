module Avro.Json.Schema exposing (..)

import Avro.Json.Value exposing (encodeDefaultValue)
import Avro.Name exposing (TypeName, contextualTypeName, parseTypeName)
import Avro.Schema exposing (Field, Schema(..), SortOrder(..))
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
                        , ( "aliases", Encode.list Encode.string f.aliases )
                        , ( "doc", encodeOrNull Encode.string f.doc )
                        , ( "order", encodeOrNull encodeSortOrder f.order )
                        , ( "type", encodeSchema f.type_ )
                        , ( "default", encodeOrNull (encodeDefaultValue f.type_) f.default )
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
                        (\name aliases fields -> Record { name = name, aliases = aliases, fields = fields, doc = Nothing })
                        decodeName
                        decodeAliases
                        (Decode.field "fields" (Decode.list decodeFields))

                "fixed" ->
                    Decode.map3
                        (\name aliases size -> Fixed { name = name, aliases = aliases, size = size })
                        decodeName
                        decodeAliases
                        (Decode.field "size" Decode.int)

                "enum" ->
                    Decode.map3
                        (\name aliases symbols -> Enum { name = name, aliases = aliases, symbols = symbols, doc = Nothing })
                        decodeName
                        decodeAliases
                        (Decode.field "symbols" (Decode.list Decode.string))

                _ ->
                    Decode.fail "Not a primitive type"
    in
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                decodeBaseType
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
                    oneOf
                        [ decodeBaseType tag
                        , decodeComplexType tag
                        ]
                )
        , Decode.map
            (\options -> Union { options = options })
            (Decode.list (Decode.lazy (\_ -> decodeSchema)))
        ]
