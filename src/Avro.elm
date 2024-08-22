module Avro exposing
    ( makeDecoder, makeEncoder
    , Environment, makeDecoderInEnvironment
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


# Working with named types

It is common when building Avro schemas traditionally to write small
schemas for parts of a larger record or set of messages. To do this,
one uses named types to create references so that schema definitions
can be simplified and reused.

In this library, this can be quite simply done for Codecs by using the
[`namedType`](Avro-Codec#namedType) function from the [`Codec`](Avro-Codec)
module.

To read and write data which is separated in this manner, one should first
construct an `Environment` for all named types used by the reader and
writer. Then, use that environment when constructing a decoder.

@docs Environment, makeDecoderInEnvironment


# Json

@docs schemaDecoder, schemaEncoder

@docs valueDecoder, valueEncoder

-}

import Avro.Codec as Codec
import Avro.Internal.Bytes as Bytes
import Avro.Internal.Deconflict exposing (deconflict)
import Avro.Internal.Overlay as Overlay
import Avro.Json.Schema as Json
import Avro.Json.Value as Json
import Avro.Schema exposing (Schema, SchemaMismatch)
import Avro.Value as Avro
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode exposing (Encoder)
import Dict
import Json.Decode
import Json.Encode


{-| Create a binary decoder for avro data given a Codec and the writer's Schema.

Fields in Avro data types are not tagged, and records can only be interpreted
knowing the exact Schema with which they were written.

Therefore, building a binary decoder not requires a Codec for the type of
interest, but also the writer of the data's [`Schema`](Avro-Schema#Schema).

-}
makeDecoder : Codec.Codec a -> Schema -> Result SchemaMismatch (Decoder a)
makeDecoder =
    makeDecoderInEnvironment (Environment [] [])


{-| Create a binary decoder for avro data given a Codec and the writer's Schema.

This function will generate a decoder after considering the Named types in the
defined set of schemas.

-}
makeDecoderInEnvironment : Environment -> Codec.Codec a -> Schema -> Result SchemaMismatch (Decoder a)
makeDecoderInEnvironment env codec writerSchema =
    let
        overlayedCodec =
            { codec | schema = Overlay.overlays codec.schema env.readerEnvironment }

        overlayedWriter =
            Overlay.overlays writerSchema env.writerEnvironment
    in
    deconflict Dict.empty overlayedCodec.schema overlayedWriter
        |> Result.map
            (\readSchema ->
                Bytes.makeDecoder Bytes.emptyEnvironment readSchema
                    |> Decode.andThen
                        (\values ->
                            case overlayedCodec.decoder values of
                                Just a ->
                                    Decode.succeed a

                                Nothing ->
                                    Decode.fail
                        )
            )


{-| Schemas which can be referred to by name in either the readers and writers.
-}
type alias Environment =
    { readerEnvironment : List Schema, writerEnvironment : List Schema }


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
