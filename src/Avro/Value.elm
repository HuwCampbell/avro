module Avro.Value exposing (Value(..))

{-| Raw Avro Values

This module defines a basic type for mapping
between encoded avro data and user defined
mappings.

# Definition

@docs Value

-}

import Avro.Name exposing (..)
import Bytes exposing (Bytes)
import Dict exposing (Dict)


{-| Avro Value types

A Value in Avro should only be interpreted within the
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
    | Record TypeName (List Value)
    | Union Int Value
    | Fixed TypeName Bytes
    | Enum Int String
