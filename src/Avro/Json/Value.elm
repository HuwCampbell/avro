module Avro.Json.Value exposing (decodeDefaultValue, decodeValue, encodeDefaultValue, encodeValue)

import Avro.Schema as Schema exposing (Schema)
import Avro.Value as Avro
import Bytes
import Bytes.Decode
import Bytes.Encode
import Char
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List


index : Int -> List a -> Maybe a
index i xs =
    List.head (List.drop i xs)


encodeDefaultValue : Schema -> Avro.Value -> Value
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


decodeDefaultValue : Schema -> Decoder Avro.Value
decodeDefaultValue schema =
    case schema of
        Schema.Union { options } ->
            case List.head options of
                Just s ->
                    decodeValue s
                        |> Decode.map (Avro.Union 0)

                Nothing ->
                    Decode.fail "Empty union schema, can't decode default value"

        _ ->
            decodeValue schema


encodeBytes : Bytes.Bytes -> Value
encodeBytes bs =
    let
        step ( n, results ) =
            if n <= 0 then
                results
                    |> List.reverse
                    |> String.fromList
                    |> Encode.string
                    |> Bytes.Decode.Done
                    |> Bytes.Decode.succeed

            else
                Bytes.Decode.map (\x -> Bytes.Decode.Loop ( n - 1, Char.fromCode x :: results )) Bytes.Decode.unsignedInt8

        result =
            Bytes.Decode.decode
                (Bytes.Decode.loop ( Bytes.width bs, [] ) step)
                bs
    in
    Maybe.withDefault Encode.null result


decodeBytes : Decoder Bytes.Bytes
decodeBytes =
    let
        toBytes s =
            String.toList s
                |> List.map (Char.toCode >> Bytes.Encode.unsignedInt8)
                |> Bytes.Encode.sequence
                |> Bytes.Encode.encode
    in
    Decode.string
        |> Decode.map toBytes


encodeValue : Schema.Schema -> Avro.Value -> Value
encodeValue schema v =
    case ( schema, v ) of
        ( Schema.Null, Avro.Null ) ->
            Encode.null

        ( Schema.Boolean, Avro.Boolean b ) ->
            Encode.bool b

        ( Schema.Int _, Avro.Int i ) ->
            Encode.int i

        ( Schema.Long _, Avro.Long l ) ->
            Encode.int l

        ( Schema.Float, Avro.Float l ) ->
            Encode.float l

        ( Schema.Double, Avro.Double l ) ->
            Encode.float l

        ( Schema.String _, Avro.String s ) ->
            Encode.string s

        ( Schema.Enum info, Avro.Enum ix ) ->
            case index ix info.symbols of
                Just s ->
                    Encode.string s

                Nothing ->
                    Encode.null

        ( Schema.Array { items }, Avro.Array ls ) ->
            Encode.list (encodeValue items) ls

        ( Schema.Map { values }, Avro.Map ls ) ->
            Encode.dict identity (encodeValue values) ls

        ( Schema.Union _, Avro.Union _ Avro.Null ) ->
            Encode.null

        ( Schema.Union { options }, Avro.Union ix ls ) ->
            case index ix options of
                Just s ->
                    Encode.object
                        [ ( (Schema.typeName s).baseName, encodeValue s ls ) ]

                Nothing ->
                    Encode.null

        ( Schema.Record { fields }, Avro.Record ls ) ->
            List.map2
                (\f i -> ( f.name, encodeValue f.type_ i ))
                fields
                ls
                |> Encode.object

        ( Schema.Bytes, Avro.Bytes bytes ) ->
            encodeBytes bytes

        ( Schema.Fixed _, Avro.Fixed _ bytes ) ->
            encodeBytes bytes

        _ ->
            Encode.null


decodeValue : Schema -> Decoder Avro.Value
decodeValue schema =
    case schema of
        Schema.Null ->
            Decode.null Avro.Null

        Schema.Boolean ->
            Decode.bool
                |> Decode.map Avro.Boolean

        Schema.Int _ ->
            Decode.int
                |> Decode.map Avro.Int

        Schema.Long _ ->
            Decode.int
                |> Decode.map Avro.Long

        Schema.Float ->
            Decode.float
                |> Decode.map Avro.Float

        Schema.Double ->
            Decode.float
                |> Decode.map Avro.Double

        Schema.String _ ->
            Decode.string
                |> Decode.map Avro.String

        Schema.Array { items } ->
            Decode.list (decodeValue items)
                |> Decode.map Avro.Array

        Schema.Map { values } ->
            Decode.dict (decodeValue values)
                |> Decode.map Avro.Map

        Schema.Union { options } ->
            let
                choice ix option =
                    case option of
                        Schema.Null ->
                            Decode.null (Avro.Union ix Avro.Null)

                        other ->
                            Decode.field
                                (Schema.typeName other).baseName
                                (decodeValue other)
                                |> Decode.map (Avro.Union ix)

                choices =
                    List.indexedMap choice options
            in
            Decode.oneOf choices

        Schema.Record { fields } ->
            let
                step acc rem =
                    case rem of
                        [] ->
                            Decode.succeed (List.reverse acc)

                        f :: xs ->
                            Decode.field f.name (decodeValue f.type_)
                                |> Decode.andThen (\a -> step (a :: acc) xs)
            in
            step [] fields
                |> Decode.map Avro.Record

        Schema.Enum { symbols } ->
            Decode.string
                |> Decode.andThen
                    (\symbol ->
                        case match symbol symbols of
                            Just ix ->
                                Decode.succeed (Avro.Enum ix)

                            Nothing ->
                                Decode.fail "Unknown enum"
                    )

        Schema.Bytes ->
            decodeBytes
                |> Decode.map Avro.Bytes

        Schema.Fixed { name } ->
            decodeBytes
                |> Decode.map (Avro.Fixed name)

        Schema.NamedType _ ->
            Decode.fail "Can't parse named type value. Normalise first"


match : a -> List a -> Maybe Int
match a =
    let
        go i input =
            case input of
                x :: xs ->
                    if a == x then
                        Just i

                    else
                        go (i + 1) xs

                _ ->
                    Nothing
    in
    go 0
