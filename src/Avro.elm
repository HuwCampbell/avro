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
import Avro.Deconflict exposing (deconflict, environmentNamesForSchema)
import Avro.Internal.Bytes as Bytes
import Avro.Json.Schema as Json
import Avro.Schema exposing (Schema, SchemaMismatch)
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.DecodeExtra as Decode
import Bytes.Encode exposing (Encoder)
import Dict
import Json.Decode
import Json.Encode
import ResultExtra exposing (traverse)
import Set


{-| Read avro data given a Codec and the writer's Schema
-}
makeDecoder : Codec.Codec a -> Schema -> Result SchemaMismatch (Decoder a)
makeDecoder =
    makeDecoderInEnvironment Dict.empty


{-| Read avro data given a Codec and the writer's Schema and an environment.
-}
makeDecoderInEnvironment : Bytes.Environment -> Codec.Codec a -> Schema -> Result SchemaMismatch (Decoder a)
makeDecoderInEnvironment env codec writerSchema =
    let
        environmentNames =
            (environmentNamesForSchema codec.schema ++ Dict.keys env)
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
makeEnvironment : List ( Schema, Schema ) -> Result SchemaMismatch Bytes.Environment
makeEnvironment schemaPairs =
    let
        environmentNames =
            List.concatMap (\( reader, _ ) -> environmentNamesForSchema reader) schemaPairs
                |> Set.fromList

        doDeconflicting ( reader, writer ) =
            deconflict environmentNames reader writer
                |> Result.map
                    (\readSchema ->
                        ( environmentNamesForSchema reader, readSchema )
                    )

        buildLazyMap readPairs =
            let
                environment _ =
                    let
                        single ( nameAndAliases, readSchema ) =
                            let
                                decoder =
                                    Decode.lazy
                                        (\_ -> Bytes.makeDecoder (environment ()) readSchema)
                            in
                            List.map (\name -> ( name, decoder )) nameAndAliases
                    in
                    List.concatMap single readPairs
                        |> Dict.fromList
            in
            environment ()
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
