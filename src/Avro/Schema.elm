module Avro.Schema exposing
    ( Schema(..)
    , Field
    , SortOrder(..)
    , typeName, withDocumentation, withAliases, withLogicalType
    )

{-| This module defines Avro Schemas


# Definition

@docs Schema

@docs Field

@docs SortOrder

@docs typeName, withDocumentation, withAliases, withLogicalType

-}

import Avro.Name exposing (..)
import Avro.Value exposing (Value)


{-| Field Sort ordering
-}
type SortOrder
    = Ascending
    | Descending
    | Ignore


{-| The Field of a Record
-}
type alias Field =
    { name : String
    , aliases : List String
    , doc : Maybe String
    , order : Maybe SortOrder
    , type_ : Schema
    , default : Maybe Value
    }


{-| An Avro Schema
-}
type Schema
    = Null
    | Boolean
    | Int { logicalType : Maybe String }
    | Long { logicalType : Maybe String }
    | Float
    | Double
    | Bytes
    | String { logicalType : Maybe String }
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
        , default : Maybe String
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

        Int _ ->
            TypeName "int" []

        Long _ ->
            TypeName "long" []

        Float ->
            TypeName "float" []

        Double ->
            TypeName "double" []

        Bytes ->
            TypeName "bytes" []

        String _ ->
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


{-| Add documentation to a Schema.

If the Schema does not support documentation (i.e, it's not a Record or Enum)
this function has no effect.

-}
withDocumentation : String -> Schema -> Schema
withDocumentation docs schema =
    case schema of
        Enum info ->
            Enum
                { info | doc = Just docs }

        Record info ->
            Record
                { info | doc = Just docs }

        _ ->
            schema


{-| Add aliases to a Schema.

If the Schema does not support aliases (i.e, it's not a named type)
this function has no effect.

-}
withAliases : List TypeName -> Schema -> Schema
withAliases aliases schema =
    case schema of
        Enum info ->
            Enum
                { info | aliases = aliases }

        Record info ->
            Record
                { info | aliases = aliases }

        Fixed info ->
            Fixed
                { info | aliases = aliases }

        _ ->
            schema


{-| Add a logical type to a Schema.

If the Schema does not support a logical type, (e.g., it's a named type)
this function has no effect.

-}
withLogicalType : String -> Schema -> Schema
withLogicalType logicalType schema =
    case schema of
        Int info ->
            Int
                { info | logicalType = Just logicalType }

        Long info ->
            Long
                { info | logicalType = Just logicalType }

        String info ->
            String
                { info | logicalType = Just logicalType }

        _ ->
            schema
