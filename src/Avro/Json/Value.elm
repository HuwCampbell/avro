module Avro.Json.Value exposing (..)

import Avro.Name exposing (TypeName, contextualTypeName)
import Avro.Schema as Schema
import Avro.Value as Avro
import Bytes
import Bytes.Decode as Bytes
import Char
import Json.Encode as Encode exposing (Value)
import List


index : Int -> List a -> Maybe a
index i xs =
    List.head (List.drop i xs)


encodeDefaultValue : Schema.Schema -> Avro.Value -> Value
encodeDefaultValue schema v =
    case ( schema, v ) of
        ( Schema.Union { options }, Avro.Union 0 ls ) ->
            case List.head options of
                Just s ->
                    encodeValue s ls

                Nothing ->
                    Encode.null

        ( Schema.Union _, _ ) ->
            Encode.null

        _ ->
            encodeValue schema v


serializeBytes : Bytes.Bytes -> Value
serializeBytes bs =
    let
        step ( n, results ) =
            if n <= 0 then
                results
                    |> List.reverse
                    |> String.fromList
                    |> Encode.string
                    |> Bytes.Done
                    |> Bytes.succeed

            else
                Bytes.map (\x -> Bytes.Loop ( n - 1, Char.fromCode x :: results )) Bytes.unsignedInt8

        result =
            Bytes.decode
                (Bytes.loop ( Bytes.width bs, [] ) step)
                bs
    in
    Maybe.withDefault Encode.null result


encodeValue : Schema.Schema -> Avro.Value -> Value
encodeValue schema v =
    case ( schema, v ) of
        ( _, Avro.Null ) ->
            Encode.null

        ( _, Avro.Boolean b ) ->
            Encode.bool b

        ( _, Avro.Int i ) ->
            Encode.int i

        ( _, Avro.Long l ) ->
            Encode.int l

        ( _, Avro.Float l ) ->
            Encode.float l

        ( _, Avro.Double l ) ->
            Encode.float l

        ( _, Avro.String s ) ->
            Encode.string s

        ( _, Avro.Enum _ s ) ->
            Encode.string s

        ( Schema.Array { items }, Avro.Array ls ) ->
            Encode.list (encodeValue items) ls

        ( _, Avro.Array _ ) ->
            Encode.null

        ( Schema.Map { values }, Avro.Map ls ) ->
            Encode.dict identity (encodeValue values) ls

        ( _, Avro.Map _ ) ->
            Encode.null

        ( Schema.Union { options }, Avro.Union ix ls ) ->
            case index ix options of
                Just s ->
                    Encode.object
                        [ ( (Schema.typeName s).baseName, encodeValue s ls ) ]

                Nothing ->
                    Encode.null

        ( _, Avro.Union _ _ ) ->
            Encode.null

        ( Schema.Record { fields }, Avro.Record ls ) ->
            List.map2
                (\f i -> ( f.name, encodeValue f.type_ i ))
                fields
                ls
                |> Encode.object

        ( _, Avro.Record _ ) ->
            Encode.null

        ( _, Avro.Bytes bytes ) ->
            serializeBytes bytes

        ( _, Avro.Fixed _ bytes ) ->
            serializeBytes bytes
