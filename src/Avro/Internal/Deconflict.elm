module Avro.Internal.Deconflict exposing (canonicalNamesForSchema, deconflict)

import Array
import Avro.Internal.ReadSchema as ReadSchema exposing (ReadSchema)
import Avro.Internal.ResultExtra exposing (traverse)
import Avro.Name as Name exposing (TypeName)
import Avro.Schema exposing (Schema(..), SchemaMismatch(..), typeName)
import Dict
import Set exposing (Set)


canonicalNamesForSchema : Schema -> List String
canonicalNamesForSchema schema =
    case nameAndAliasesFor schema of
        Just { name, aliases } ->
            name
                :: aliases
                |> List.map (Name.canonicalName >> .baseName)

        Nothing ->
            []


nameAndAliasesFor : Schema -> Maybe { name : TypeName, aliases : List TypeName }
nameAndAliasesFor reader =
    case reader of
        Enum info ->
            Just { name = info.name, aliases = info.aliases }

        Record info ->
            Just { name = info.name, aliases = info.aliases }

        Fixed info ->
            Just { name = info.name, aliases = info.aliases }

        _ ->
            Nothing


{-| A function to deconflict a reader and writer Schema

This allows values to be read by a different schema from
whence it was written.

-}
deconflict : Set String -> Schema -> Schema -> Result SchemaMismatch ReadSchema
deconflict environmentNames readSchema writerSchema =
    let
        basicError =
            Err <|
                TypeMismatch readSchema writerSchema
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

        Bytes _ ->
            case writerSchema of
                Bytes _ ->
                    Ok ReadSchema.Bytes

                String _ ->
                    Ok ReadSchema.Bytes

                _ ->
                    basicError

        String _ ->
            case writerSchema of
                Bytes _ ->
                    Ok ReadSchema.String

                String _ ->
                    Ok ReadSchema.String

                _ ->
                    basicError

        Array readElem ->
            case writerSchema of
                Array writeElem ->
                    deconflict environmentNames readElem.items writeElem.items
                        |> Result.map (\items -> ReadSchema.Array { items = items })

                _ ->
                    basicError

        Map readElem ->
            case writerSchema of
                Map writeElem ->
                    deconflict environmentNames readElem.values writeElem.values
                        |> Result.map (\values -> ReadSchema.Map { values = values })

                _ ->
                    basicError

        Record readInfo ->
            case writerSchema of
                Record writeInfo ->
                    let
                        nestedEnvironment =
                            Set.union environmentNames (Set.fromList (canonicalNamesForSchema readSchema))

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
                                                                    Err (MissingField readInfo.name unwritten.name)
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
                                            deconflict nestedEnvironment r.type_ w.type_
                                                |> Result.andThen
                                                    (\dr ->
                                                        let
                                                            readField =
                                                                ReadSchema.ReadField r.name dr (Just ix)
                                                        in
                                                        step ws { written = readField :: acc.written, left = more }
                                                    )
                                                |> Result.mapError
                                                    (FieldMismatch readInfo.name w.name)

                                        Nothing ->
                                            deconflict nestedEnvironment w.type_ w.type_
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
                --
                -- Two pass search algorithm. First we search by the names of the elements,
                -- then we search by whether they can be deconflicted.
                resolveBranch branchWriter continuation =
                    case find (compatiblyNamed (typeName branchWriter)) readInfo.options of
                        Just ( r, ix ) ->
                            deconflict environmentNames r branchWriter
                                |> Result.andThen (continuation ix)

                        Nothing ->
                            case findOk (\r -> deconflict environmentNames r branchWriter) readInfo.options of
                                Just ( r, ix ) ->
                                    continuation ix r

                                Nothing ->
                                    Err <| MissingUnion (typeName branchWriter)

                compatiblyNamed writeInfo reader =
                    nameAndAliasesFor reader
                        |> Maybe.withDefault { name = typeName reader, aliases = [] }
                        |> (\fieldInfo -> Name.compatibleNames fieldInfo { name = writeInfo })
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
                                    resolveBranch w (\ix dr -> step ws { written = ( ix, dr ) :: acc.written })
                    in
                    step writerInfo.options { written = [] }

                singlular ->
                    resolveBranch singlular (\ix a -> Ok (ReadSchema.AsUnion ix a))

        Enum readInfo ->
            case writerSchema of
                Enum writeInfo ->
                    let
                        match writeSymbol =
                            case find ((==) writeSymbol) readInfo.symbols of
                                Just ( _, ix ) ->
                                    Ok ix

                                Nothing ->
                                    case readInfo.default of
                                        Just def ->
                                            case find ((==) def) readInfo.symbols of
                                                Just ( _, ix ) ->
                                                    Ok ix

                                                Nothing ->
                                                    Err <| MissingSymbol def

                                        Nothing ->
                                            Err <| MissingSymbol writeSymbol

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
                        if Set.member (Name.canonicalName writerName).baseName environmentNames then
                            Ok (ReadSchema.NamedType readerName)

                        else
                            Err (NamedTypeUnresolved writerName)

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


findOk : (a -> Result e b) -> List a -> Maybe ( b, Int )
findOk f =
    let
        go i input =
            case input of
                x :: xs ->
                    case f x of
                        Ok b ->
                            Just ( b, i )

                        Err _ ->
                            go (i + 1) xs

                _ ->
                    Nothing
    in
    go 0
