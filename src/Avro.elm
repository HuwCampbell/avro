module Avro exposing
    ( makeDecoder, makeEncoder
    , makeEnvironment, makeDecoderInEnvironment
    , schemaDecoder, schemaEncoder
    )

{-| This module contains top level functions for
converting Avro data to and from typed Elm values.

It is designed to be used with the
[`Avro.Codec`](Avro-Codec) module.

One should construct a [`Codec`](Avro-Codec#Codec), then,
use the functions below to read and write Avro encoded binary data.


# Parsing and writing Avro data

@docs makeDecoder, makeEncoder


# Parsing data with named types

One can use named types to create references so
that schema definitions can be simplified and reused.

This should be used with [`namedType`](Avro-Codec#namedType)
from the [`Codec`](Avro-Codec) module.

One should first construct an `Environment` for all
named types, using the schemas they were written
with and with which they will be read. Then, use that
environment when constructing a decoder.

@docs makeEnvironment, makeDecoderInEnvironment


# Json

@docs schemaDecoder, schemaEncoder

-}

import Avro.Codec as Codec
import Avro.Deconflict exposing (deconflict)
import Avro.Internal.Bytes as Bytes
import Avro.Json.Schema as Json
import Avro.Name as Name
import Avro.Schema as Schema exposing (Schema, SchemaMismatch)
import Bytes.Decode exposing (Decoder)
import Bytes.Encode exposing (Encoder)
import Dict
import Json.Decode
import Json.Encode


{-| Read avro data given a Codec and the writer's Schema
-}
makeDecoder : Codec.Codec a -> Schema -> Result SchemaMismatch (Decoder a)
makeDecoder =
    makeDecoderInEnvironment Dict.empty


{-| Read avro data given a Codec and the writer's Schema and an environment.
-}
makeDecoderInEnvironment : Bytes.Environment -> Codec.Codec a -> Schema -> Result SchemaMismatch (Decoder a)
makeDecoderInEnvironment env codec writerSchema =
    deconflict codec.schema writerSchema
        |> Result.map
            (\readSchema ->
                Bytes.makeDecoder env readSchema
                    |> Bytes.Decode.andThen
                        (\values ->
                            case codec.decoder values of
                                Just a ->
                                    Bytes.Decode.succeed a

                                Nothing ->
                                    Bytes.Decode.fail
                        )
            )


{-| Build an environment from a list of reader and writer schemas.
-}
makeEnvironment : List ( Schema, Schema ) -> Result SchemaMismatch Bytes.Environment
makeEnvironment =
    let
        go acc more =
            case more of
                ( reader, writer ) :: xs ->
                    deconflict reader writer
                        |> Result.andThen (\e -> go (Dict.insert (Schema.typeName reader |> Name.canonicalName |> .baseName) (Bytes.makeDecoder acc e) acc) xs)

                _ ->
                    Ok acc
    in
    go Dict.empty


{-| Make a binary encoder for data using an Avro Codec
-}
makeEncoder : Codec.Codec a -> a -> Encoder
makeEncoder codec =
    Bytes.encodeValue << codec.writer


{-| JSON decoder for an Avro Schema
-}
schemaDecoder : Json.Decode.Decoder Schema
schemaDecoder =
    Json.decodeSchema


{-| JSON encoder for an Avro Schema
-}
schemaEncoder : Schema -> Json.Encode.Value
schemaEncoder =
    Json.encodeSchema
