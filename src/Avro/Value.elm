module Avro.Value exposing (Value(..))

{-| This module defines a basic type for mapping
between encoded avro data and user defined
mappings.

One usually doesn't need to use this module,
as decoding to an Elm value via a [`Codec`](Avro-Codec#Codec)
is usually more appropriate.


# Definition

@docs Value

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)


{-| Avro Value types

A Value in Avro can only be interpreted within the
context of a Schema.

For example, records are written in field order.

-}
type Value
    = Null
    | Boolean Bool
    | Int Int
    | Long Int
    | Float Float
    | Double Float
    | Bytes Bytes
    | String String
    | Array (List Value)
    | Map (Dict String Value)
    | Record (List Value)
    | Union Int Value
    | Fixed Bytes
    | Enum Int
