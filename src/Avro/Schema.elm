module Avro.Schema exposing
    ( Schema(..)
    , Field
    , typeName
    , deconflict
    )

{-| This module defines Avro Schemas


# Definition

@docs Schema

@docs Field

@docs typeName

@docs deconflict

-}

import Avro.Internal.ReadSchema as ReadSchema exposing (ReadSchema)
import Avro.Name exposing (..)
import Avro.Value exposing (Value)
import Dict


{-| The Field of a Record
-}
type alias Field =
    { name : String
    , aliases : List String
    , doc : Maybe String
    , order : Maybe Order
    , type_ : Schema
    , default : Maybe Value
    }


{-| An Avro Schema
-}
type Schema
    = Null
    | Boolean
    | Int
    | Long
    | Float
    | Double
    | Bytes
    | String
    | Array { items : Schema }
    | Map { values : Schema }
    | NamedType TypeName
    | Record
        { name : TypeName
        , aliases : List TypeName
        , doc : Maybe String
        , fields : List Field
        }
    | Enum
        { name : TypeName
        , aliases : List TypeName
        , doc : Maybe String
        , symbols : List String
        }
    | Union { options : List Schema }
    | Fixed
        { name : TypeName
        , aliases : List TypeName
        , size : Int
        }


{-| An Avro Schema
-}
typeName : Schema -> TypeName
typeName s =
    case s of
        Null ->
            simpleName "null"

        Boolean ->
            simpleName "boolean"

        Int ->
            simpleName "int"

        Long ->
            simpleName "long"

        Float ->
            simpleName "float"

        Double ->
            simpleName "double"

        Bytes ->
            simpleName "bytes"

        String ->
            simpleName "string"

        Array _ ->
            simpleName "array"

        Map _ ->
            simpleName "map"

        Union _ ->
            simpleName "union"

        Fixed _ ->
            simpleName "fixed"

        NamedType name ->
            name

        Record rs ->
            rs.name

        Enum e ->
            e.name


{-| A function to deconflict a reader and writer Schema

This allows values to be read by a different schema from
whence written.

-}
deconflict : Schema -> Schema -> Maybe ReadSchema
deconflict readSchema writerSchema =
    case readSchema of
        Null ->
            case writerSchema of
                Null ->
                    Just ReadSchema.Null

                _ ->
                    Nothing

        Boolean ->
            case writerSchema of
                Boolean ->
                    Just ReadSchema.Boolean

                _ ->
                    Nothing

        Int ->
            case writerSchema of
                Int ->
                    Just ReadSchema.Int

                _ ->
                    Nothing

        Long ->
            case writerSchema of
                Int ->
                    Just ReadSchema.IntAsLong

                Long ->
                    Just ReadSchema.Long

                _ ->
                    Nothing

        Float ->
            case writerSchema of
                Int ->
                    Just ReadSchema.IntAsFloat

                Long ->
                    Just ReadSchema.LongAsFloat

                Float ->
                    Just ReadSchema.Float

                _ ->
                    Nothing

        Double ->
            case writerSchema of
                Int ->
                    Just ReadSchema.IntAsDouble

                Long ->
                    Just ReadSchema.LongAsDouble

                Float ->
                    Just ReadSchema.FloatAsDouble

                Double ->
                    Just ReadSchema.Double

                _ ->
                    Nothing

        Bytes ->
            case writerSchema of
                Bytes ->
                    Just ReadSchema.Bytes

                _ ->
                    Nothing

        String ->
            case writerSchema of
                String ->
                    Just ReadSchema.String

                _ ->
                    Nothing

        Array readElem ->
            case writerSchema of
                Array writeElem ->
                    deconflict readElem.items writeElem.items
                        |> Maybe.map (\items -> ReadSchema.Array { items = items })

                _ ->
                    Nothing

        Map readElem ->
            case writerSchema of
                Map writeElem ->
                    deconflict readElem.values writeElem.values
                        |> Maybe.map (\values -> ReadSchema.Map { values = values })

                _ ->
                    Nothing

        Record readInfo ->
            case writerSchema of
                Record writeInfo ->
                    let
                        matching w ( r, _ ) =
                            r.name
                                == w.name
                                || List.any (\ali -> ali == w.name) r.aliases

                        step work acc =
                            case work of
                                [] ->
                                    let
                                        maybeDefaults =
                                            List.foldl
                                                (\( unwritten, ix ) ->
                                                    Maybe.andThen
                                                        (\known ->
                                                            unwritten.default
                                                                |> Maybe.map (\d -> Dict.insert ix d known)
                                                        )
                                                )
                                                (Just Dict.empty)
                                                acc.left
                                    in
                                    maybeDefaults
                                        |> Maybe.map
                                            (\defaults ->
                                                ReadSchema.Record
                                                    { name = readInfo.name
                                                    , fields = List.reverse acc.written
                                                    , defaults = defaults
                                                    }
                                            )

                                w :: ws ->
                                    case pick (matching w) acc.left of
                                        Just ( ( r, ix ), more ) ->
                                            deconflict r.type_ w.type_
                                                |> Maybe.andThen
                                                    (\dr ->
                                                        let
                                                            readField =
                                                                ReadSchema.ReadField r.name dr (Just ix)
                                                        in
                                                        step ws { written = readField :: acc.written, left = more }
                                                    )

                                        Nothing ->
                                            deconflict w.type_ w.type_
                                                |> Maybe.andThen
                                                    (\dr ->
                                                        let
                                                            readField =
                                                                ReadSchema.ReadField w.name dr Nothing
                                                        in
                                                        step ws { written = readField :: acc.written, left = acc.left }
                                                    )
                    in
                    step writeInfo.fields { written = [], left = List.indexedMap (\a b -> ( b, a )) readInfo.fields }

                _ ->
                    Nothing

        Union readInfo ->
            let
                matching w r =
                    typeName w
                        == typeName r
            in
            case writerSchema of
                Union writerInfo ->
                    let
                        step work acc =
                            case work of
                                [] ->
                                    Just
                                        (ReadSchema.Union { options = List.reverse acc.written })

                                w :: ws ->
                                    find (matching w) readInfo.options
                                        |> Maybe.andThen
                                            (\( r, ix ) ->
                                                deconflict r w
                                                    |> Maybe.andThen
                                                        (\dr ->
                                                            step ws { written = ( ix, dr ) :: acc.written }
                                                        )
                                            )
                    in
                    step writerInfo.options { written = [] }

                other ->
                    pick (matching other) readInfo.options
                        |> Maybe.andThen
                            (\( r, _ ) ->
                                deconflict r other
                                    |> Maybe.map ReadSchema.AsUnion
                            )

        Enum readInfo ->
            case writerSchema of
                Enum writeInfo ->
                    if readInfo.symbols == writeInfo.symbols then
                        Just <|
                            ReadSchema.Enum
                                { name = readInfo.name
                                , symbols = readInfo.symbols
                                }

                    else
                        Nothing

                _ ->
                    Nothing

        Fixed readInfo ->
            case writerSchema of
                Fixed writeInfo ->
                    if readInfo.size == writeInfo.size then
                        Just <|
                            ReadSchema.Fixed
                                { name = readInfo.name
                                , size = readInfo.size
                                }

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


pick : (a -> Bool) -> List a -> Maybe ( a, List a )
pick f =
    let
        go seen input =
            case input of
                x :: xs ->
                    if f x then
                        Just ( x, List.append (List.reverse seen) xs )

                    else
                        go (x :: seen) xs

                _ ->
                    Nothing
    in
    go []


find : (a -> Bool) -> List a -> Maybe ( a, Int )
find f =
    let
        go i input =
            case input of
                x :: xs ->
                    if f x then
                        Just ( x, i )

                    else
                        go (i + 1) xs

                _ ->
                    Nothing
    in
    go 0
