module Avro.Deconflict exposing (SchemaMismatch(..), deconflict)

import Array
import Avro.Name exposing (TypeName)
import Avro.ReadSchema as ReadSchema exposing (ReadSchema)
import Avro.Schema exposing (Schema(..), typeName)
import Dict
import ResultExtra exposing (traverse)


type SchemaMismatch
    = SchemaTypeMismatch Schema Schema
    | SchemaMissingField String
    | SchemaMissingUnion TypeName
    | SchemaMissingEnum String


{-| A function to deconflict a reader and writer Schema

This allows values to be read by a different schema from
whence it was written.

-}
deconflict : Schema -> Schema -> Result SchemaMismatch ReadSchema
deconflict readSchema writerSchema =
    let
        basicError =
            Err <|
                SchemaTypeMismatch readSchema writerSchema
    in
    case readSchema of
        Null ->
            case writerSchema of
                Null ->
                    Ok ReadSchema.Null

                _ ->
                    basicError

        Boolean ->
            case writerSchema of
                Boolean ->
                    Ok ReadSchema.Boolean

                _ ->
                    basicError

        Int _ ->
            case writerSchema of
                Int _ ->
                    Ok ReadSchema.Int

                _ ->
                    basicError

        Long _ ->
            case writerSchema of
                Int _ ->
                    Ok ReadSchema.IntAsLong

                Long _ ->
                    Ok ReadSchema.Long

                _ ->
                    basicError

        Float ->
            case writerSchema of
                Int _ ->
                    Ok ReadSchema.IntAsFloat

                Long _ ->
                    Ok ReadSchema.LongAsFloat

                Float ->
                    Ok ReadSchema.Float

                _ ->
                    basicError

        Double ->
            case writerSchema of
                Int _ ->
                    Ok ReadSchema.IntAsDouble

                Long _ ->
                    Ok ReadSchema.LongAsDouble

                Float ->
                    Ok ReadSchema.FloatAsDouble

                Double ->
                    Ok ReadSchema.Double

                _ ->
                    basicError

        Bytes ->
            case writerSchema of
                Bytes ->
                    Ok ReadSchema.Bytes

                _ ->
                    basicError

        String _ ->
            case writerSchema of
                String _ ->
                    Ok ReadSchema.String

                _ ->
                    basicError

        Array readElem ->
            case writerSchema of
                Array writeElem ->
                    deconflict readElem.items writeElem.items
                        |> Result.map (\items -> ReadSchema.Array { items = items })

                _ ->
                    basicError

        Map readElem ->
            case writerSchema of
                Map writeElem ->
                    deconflict readElem.values writeElem.values
                        |> Result.map (\values -> ReadSchema.Map { values = values })

                _ ->
                    basicError

        Record readInfo ->
            case writerSchema of
                Record writeInfo ->
                    let
                        matching w ( r, _ ) =
                            r.name
                                == w.name
                                || List.member w.name r.aliases

                        step work acc =
                            case work of
                                [] ->
                                    let
                                        maybeDefaults =
                                            List.foldl
                                                (\( unwritten, ix ) ->
                                                    Result.andThen
                                                        (\known ->
                                                            case unwritten.default of
                                                                Just d ->
                                                                    Ok (Dict.insert ix d known)

                                                                Nothing ->
                                                                    Err (SchemaMissingField unwritten.name)
                                                        )
                                                )
                                                (Ok Dict.empty)
                                                acc.left
                                    in
                                    maybeDefaults
                                        |> Result.map
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
                                                |> Result.andThen
                                                    (\dr ->
                                                        let
                                                            readField =
                                                                ReadSchema.ReadField r.name dr (Just ix)
                                                        in
                                                        step ws { written = readField :: acc.written, left = more }
                                                    )

                                        Nothing ->
                                            deconflict w.type_ w.type_
                                                |> Result.andThen
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
                    basicError

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
                                    Ok <|
                                        ReadSchema.Union { options = List.reverse acc.written }

                                w :: ws ->
                                    case find (matching w) readInfo.options of
                                        Just ( r, ix ) ->
                                            deconflict r w
                                                |> Result.andThen
                                                    (\dr ->
                                                        step ws { written = ( ix, dr ) :: acc.written }
                                                    )

                                        Nothing ->
                                            Err <| SchemaMissingUnion (typeName w)
                    in
                    step writerInfo.options { written = [] }

                other ->
                    case find (matching other) readInfo.options of
                        Just ( r, ix ) ->
                            deconflict r other
                                |> Result.map (ReadSchema.AsUnion ix)

                        Nothing ->
                            Err <| SchemaMissingUnion (typeName other)

        Enum readInfo ->
            case writerSchema of
                Enum writeInfo ->
                    let
                        match writeSymbol =
                            case find (\r -> r == writeSymbol) readInfo.symbols of
                                Just ( _, ix ) ->
                                    Ok ix

                                Nothing ->
                                    case readInfo.default of
                                        Just def ->
                                            case find (\r -> r == def) readInfo.symbols of
                                                Just ( _, ix ) ->
                                                    Ok ix

                                                Nothing ->
                                                    Err <| SchemaMissingEnum def

                                        Nothing ->
                                            Err <| SchemaMissingEnum writeSymbol

                        lined =
                            traverse match writeInfo.symbols
                    in
                    Result.map
                        (\good -> ReadSchema.Enum { name = readInfo.name, symbols = Array.fromList good })
                        lined

                _ ->
                    basicError

        Fixed readInfo ->
            case writerSchema of
                Fixed writeInfo ->
                    if readInfo.size == writeInfo.size then
                        Ok <|
                            ReadSchema.Fixed
                                { name = readInfo.name
                                , size = readInfo.size
                                }

                    else
                        basicError

                _ ->
                    basicError

        NamedType readerName ->
            case writerSchema of
                NamedType writerName ->
                    if readerName == writerName then
                        Ok (ReadSchema.NamedType readerName)

                    else
                        basicError

                _ ->
                    basicError


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
