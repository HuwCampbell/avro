module Avro.Deconflict exposing (deconflict)

import Avro.ReadSchema as ReadSchema exposing (ReadSchema)
import Avro.Schema exposing (Schema(..), typeName)
import Dict


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

        Int _ ->
            case writerSchema of
                Int _ ->
                    Just ReadSchema.Int

                _ ->
                    Nothing

        Long _ ->
            case writerSchema of
                Int _ ->
                    Just ReadSchema.IntAsLong

                Long _ ->
                    Just ReadSchema.Long

                _ ->
                    Nothing

        Float ->
            case writerSchema of
                Int _ ->
                    Just ReadSchema.IntAsFloat

                Long _ ->
                    Just ReadSchema.LongAsFloat

                Float ->
                    Just ReadSchema.Float

                _ ->
                    Nothing

        Double ->
            case writerSchema of
                Int _ ->
                    Just ReadSchema.IntAsDouble

                Long _ ->
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
                                || List.member w.name r.aliases

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
                    find (matching other) readInfo.options
                        |> Maybe.andThen
                            (\( r, ix ) ->
                                deconflict r other
                                    |> Maybe.map (ReadSchema.AsUnion ix)
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

        NamedType readerName ->
            case writerSchema of
                NamedType writerName ->
                    if readerName == writerName then
                        Just (ReadSchema.NamedType readerName)

                    else
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
