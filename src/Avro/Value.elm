module Avro.Value exposing (..)

import Avro.Name exposing (..)
import Bytes exposing (Bytes)
import Dict exposing (Dict)


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
    | Record TypeName (List (String, Value))
    | Union Int Value
    | Fixed TypeName Bytes
    | Enum String
