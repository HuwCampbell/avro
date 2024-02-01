module Avro exposing
    ( makeDecoder, makeEncoder
    , schemaDecoder, schemaEncoder
    )

{-| This module contains top level functions for
converting Avro data to and from typed Elm values.

To interface with this library, one should build
a [`Codec`](Avro-Codec#Codec).


# Parsing and Writing Avro Data

@docs makeDecoder, makeEncoder


# Json

@docs schemaDecoder, schemaEncoder

-}

import Avro.Codec as Codec
import Avro.Internal.Deconflict exposing (deconflict)
import Avro.Internal.Parser as Parser
import Avro.Json.Schema as Json
import Avro.Schema exposing (Schema)
import Bytes.Decode exposing (Decoder)
import Bytes.Encode exposing (Encoder)
import Dict
import Json.Decode
import Json.Encode


{-| Read avro data given a Codec and the writer's Schema
-}
makeDecoder : Codec.Codec a -> Schema -> Maybe (Decoder a)
makeDecoder codec writerSchema =
    deconflict codec.schema writerSchema
        |> Maybe.map
            (\readSchema ->
                Parser.makeDecoder Dict.empty readSchema
                    |> Bytes.Decode.andThen
                        (\values ->
                            case codec.decoder values of
                                Just a ->
                                    Bytes.Decode.succeed a

                                Nothing ->
                                    Bytes.Decode.fail
                        )
            )


{-| Read avro data given a Codec and the writer's Schema
-}
makeEncoder : Codec.Codec a -> a -> Encoder
makeEncoder codec data =
    Parser.encodeValue
        (codec.writer data)


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
