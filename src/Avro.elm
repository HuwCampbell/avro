module Avro exposing
    ( makeDecoder, makeEncoder
    , Environment, makeEnvironment, makeDecoderInEnvironment
    , schemaDecoder, schemaEncoder
    , valueDecoder, valueEncoder
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
named types, using the schemas they will be read with
and with which they were written. Then, use that
environment when constructing a decoder.

@docs Environment, makeEnvironment, makeDecoderInEnvironment


# Json

@docs schemaDecoder, schemaEncoder

@docs valueDecoder, valueEncoder

-}

import Avro.Codec as Codec
import Avro.Internal.Bytes as Bytes
import Avro.Internal.Deconflict exposing (canonicalNamesForSchema, deconflict)
import Avro.Internal.ResultExtra exposing (traverse)
import Avro.Json.Schema as Json
import Avro.Json.Value as Json
import Avro.Schema exposing (Schema, SchemaMismatch)
import Avro.Value as Avro
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode exposing (Encoder)
import Json.Decode
import Json.Encode
import Set


{-| Create a binary decoder for avro data given a Codec and the writer's Schema.

Fields in Avro data types are not tagged, and records can only be interpreted
knowing the exact Schema with which they were written.

Therefore, building a binary decoder not requires a Codec for the type of
interest, but also the writer of the data's [`Schema`](Avro-Schema#Schema).

-}
makeDecoder : Codec.Codec a -> Schema -> Result SchemaMismatch (Decoder a)
makeDecoder =
    makeDecoderInEnvironment Bytes.emptyEnvironment


{-| A Schema environment used for finding schemas by name
-}
type alias Environment =
    Bytes.Environment


{-| Read avro data given a Codec and the writer's Schema and an environment.
-}
makeDecoderInEnvironment : Environment -> Codec.Codec a -> Schema -> Result SchemaMismatch (Decoder a)
makeDecoderInEnvironment env codec writerSchema =
    let
        environmentNames =
            Bytes.environmentNames env
                |> Set.fromList
    in
    deconflict environmentNames codec.schema writerSchema
        |> Result.map
            (\readSchema ->
                Bytes.makeDecoder env readSchema
                    |> Decode.andThen
                        (\values ->
                            case codec.decoder values of
                                Just a ->
                                    Decode.succeed a

                                Nothing ->
                                    Decode.fail
                        )
            )


{-| Build an environment from a list of reader and writer schemas.
-}
makeEnvironment : List ( Schema, Schema ) -> Result SchemaMismatch Environment
makeEnvironment schemaPairs =
    let
        environmentNames =
            List.concatMap (\( reader, _ ) -> canonicalNamesForSchema reader) schemaPairs
                |> Set.fromList

        doDeconflicting ( reader, writer ) =
            deconflict environmentNames reader writer
                |> Result.map
                    (\readSchema ->
                        List.map (\name -> ( name, readSchema )) (canonicalNamesForSchema reader)
                    )

        buildLazyMap readPairs =
            Bytes.makeDecoderEnvironment (List.concat readPairs)
    in
    traverse doDeconflicting schemaPairs
        |> Result.map buildLazyMap


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


{-| JSON decoder for an Avro Value
-}
valueDecoder : Schema -> Json.Decode.Decoder Avro.Value
valueDecoder =
    Json.decodeValue


{-| JSON encoder for an Avro Value
-}
valueEncoder : Schema -> Avro.Value -> Json.Encode.Value
valueEncoder =
    Json.encodeValue
