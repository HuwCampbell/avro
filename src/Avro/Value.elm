module Avro.Value exposing (Value(..))

{-| This module defines a representation of an Avro value.

One usually doesn't need to use this module,
as decoding straight to an Elm domain type via a [`Codec`](Avro-Codec#Codec)
is better in most situations.

This type is exposed though as it can be used to build a
render function for Avro values of arbitrary types, which
could be useful for a monitoring tool.


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
