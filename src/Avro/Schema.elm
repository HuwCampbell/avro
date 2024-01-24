module Avro.Schema exposing
    ( Schema(..)
    , Field
    , typeName
    )

{-| This module defines Avro Schemas


# Definition

@docs Schema

@docs Field

@docs typeName

-}

import Avro.Internal.ReadSchema as ReadSchema exposing (ReadSchema)
import Avro.Name exposing (..)
import Avro.Value exposing (Value)
import Dict


{-| The Field of a Record
-}
type alias Field =
    { name : String
    , aliases : List String
    , doc : Maybe String
    , order : Maybe Order
    , type_ : Schema
    , default : Maybe Value
    }


{-| An Avro Schema
-}
type Schema
    = Null
    | Boolean
    | Int
    | Long
    | Float
    | Double
    | Bytes
    | String
    | Array { items : Schema }
    | Map { values : Schema }
    | NamedType TypeName
    | Record
        { name : TypeName
        , aliases : List TypeName
        , doc : Maybe String
        , fields : List Field
        }
    | Enum
        { name : TypeName
        , aliases : List TypeName
        , doc : Maybe String
        , symbols : List String
        }
    | Union { options : List Schema }
    | Fixed
        { name : TypeName
        , aliases : List TypeName
        , size : Int
        }


{-| An Avro Schema
-}
typeName : Schema -> TypeName
typeName s =
    case s of
        Null ->
            TypeName "null" []

        Boolean ->
            TypeName "boolean" []

        Int ->
            TypeName "int" []

        Long ->
            TypeName "long" []

        Float ->
            TypeName "float" []

        Double ->
            TypeName "double" []

        Bytes ->
            TypeName "bytes" []

        String ->
            TypeName "string" []

        Array _ ->
            TypeName "array" []

        Map _ ->
            TypeName "map" []

        Union _ ->
            TypeName "union" []

        Fixed _ ->
            TypeName "fixed" []

        NamedType name ->
            name

        Record rs ->
            rs.name

        Enum e ->
            e.name
