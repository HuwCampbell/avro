module Avro.Schema exposing
    ( Schema(..)
    , Field
    , SortOrder(..)
    , typeName, withDocumentation, withAliases, withLogicalType
    , SchemaMismatch(..), showSchemaMismatch
    )

{-| This module defines core Avro Schema types and functions
for working with them.


# Definition

@docs Schema

@docs Field

@docs SortOrder


# Helpers

@docs typeName, withDocumentation, withAliases, withLogicalType


# Error handling

@docs SchemaMismatch, showSchemaMismatch

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
        { logicalType : Maybe String
        }
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
        , logicalType : Maybe String
        }


{-| Get the TypeName for an Avro Schema

For primitive types, this is an unqualified name, but for
complex types it may be qualified.

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

        Bytes _ ->
            TypeName "bytes" []

        String _ ->
            TypeName "string" []

        Array _ ->
            TypeName "array" []

        Map _ ->
            TypeName "map" []

        Union _ ->
            TypeName "union" []

        NamedType name ->
            name

        Fixed info ->
            info.name

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

        Bytes info ->
            Bytes
                { info | logicalType = Just logicalType }

        Fixed info ->
            Fixed
                { info | logicalType = Just logicalType }

        String info ->
            String
                { info | logicalType = Just logicalType }

        _ ->
            schema


{-| Errors which can occur when trying to read Avro with
an incompatible Schema.
-}
type SchemaMismatch
    = TypeMismatch Schema Schema
    | MissingField TypeName String
    | FieldMismatch TypeName String SchemaMismatch
    | MissingUnion TypeName
    | MissingSymbol String
    | NamedTypeUnresolved TypeName


{-| Display a Schema mismatch error.
-}
showSchemaMismatch : SchemaMismatch -> String
showSchemaMismatch sm =
    case sm of
        TypeMismatch r w ->
            String.join "\n"
                [ "Schema type mismatch,"
                , "the reader type was " ++ (typeName r).baseName
                , "and the writer type was " ++ (typeName w).baseName
                , "these should match"
                ]

        FieldMismatch recordName fld err ->
            String.join "\n"
                [ showSchemaMismatch err
                , "in field " ++ fld
                , "of record: " ++ recordName.baseName
                ]

        MissingField recordName fld ->
            String.join "\n"
                [ "Missing field: " ++ fld
                , "of record: " ++ recordName.baseName
                ]

        MissingUnion typ ->
            String.join "\n"
                [ "Missing type in Union: " ++ typ.baseName
                ]

        MissingSymbol s ->
            String.join "\n"
                [ "Missing symbol in Enum: " ++ s
                ]

        NamedTypeUnresolved typ ->
            String.join "\n"
                [ "A named type could not be found in the environment: " ++ typ.baseName
                ]
