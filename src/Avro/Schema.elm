module Avro.Schema exposing
    ( Schema(..)
    , Field
    , SortOrder(..)
    , typeName, withDocumentation, withAliases, withLogicalType
    , SchemaMismatch(..), showSchemaMismatch
    , SchemaInvalid(..), showSchemaInvalid, validateSchema
    , canonicalise
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


# Schema Validation

@docs SchemaInvalid, showSchemaInvalid, validateSchema


# Canonical Form

@docs canonicalise

-}

import Avro.Internal.ResultExtra as Result
import Avro.Name as Name exposing (TypeName, canonicalName)
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
    | FixedWrongSize TypeName Int Int


{-| Display a Schema mismatch error.
-}
showSchemaMismatch : SchemaMismatch -> String
showSchemaMismatch sm =
    case sm of
        TypeMismatch r w ->
            String.join "\n"
                [ "Schema type mismatch,"
                , "the reader type was " ++ (typeName r).baseName ++ ","
                , "and the writer type was " ++ (typeName w).baseName ++ ","
                , "these should match"
                ]

        FieldMismatch recordName fld err ->
            String.join "\n"
                [ showSchemaMismatch err
                , "in field " ++ fld ++ ","
                , "of record: " ++ recordName.baseName ++ "."
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

        FixedWrongSize typ readSize writeSize ->
            String.join "\n"
                [ "Fixed type size mismatch for" ++ typ.baseName ++ ","
                , "the reader size was " ++ String.fromInt readSize ++ ","
                , "and the writer size was " ++ String.fromInt writeSize ++ ","
                , "these should match."
                ]


{-| Errors which can occur when a Schema is poorly described.
-}
type SchemaInvalid
    = SchemaNestedUnion
    | SchemaIdenticalNamesInUnion TypeName
    | SchemaHasInvalidFieldName TypeName String String
    | SchemaHasInvalidName TypeName String
    | SchemaHasDuplicateEnumValue TypeName String
    | SchemaEnumDefaultNotFound TypeName String


{-| Display a Schema invalid error.
-}
showSchemaInvalid : SchemaInvalid -> String
showSchemaInvalid si =
    case si of
        SchemaNestedUnion ->
            String.join "\n"
                [ "The Schema contains a union directly within another Union."
                ]

        SchemaIdenticalNamesInUnion tn ->
            String.join "\n"
                [ "A Union contains more than one variant of the same type,"
                , "  type: " ++ (canonicalName tn).baseName
                ]

        SchemaHasInvalidFieldName tn field err ->
            String.join "\n"
                [ "A Record contains an invalid field name,"
                , "  record: " ++ (canonicalName tn).baseName
                , "  field: " ++ field
                , "  error: " ++ err
                ]

        SchemaHasInvalidName tn err ->
            String.join "\n"
                [ "The Schema has an invalid name,"
                , "  field: " ++ (canonicalName tn).baseName
                , "  error: " ++ err
                ]

        SchemaHasDuplicateEnumValue tn err ->
            String.join "\n"
                [ "An Enum contains a duplicate value,"
                , "  enum: " ++ (canonicalName tn).baseName
                , "  variant: " ++ err
                ]

        SchemaEnumDefaultNotFound tn def ->
            String.join "\n"
                [ "An Enum's default is non a member of the enum,"
                , "  enum: " ++ (canonicalName tn).baseName
                , "  default: " ++ def
                ]


{-| Validates an Avro schema.

Schema's produced using the Codec module or parsed from JSON are typically
valid, but there are some ways to create invalid Schemas which may not be
well received by other implementations.

This function checks for the most common mistakes:

  - Unions must not directly contain other unions,
  - Unions must not ambiguous (contain more than one schemas with the same
    name or simple type),
  - Enums must not have duplicate values,
  - Enums with a default values must include the value in their options, and
  - That type names and record field names are valid.

Other things which are currently not covered by this function:

  - That named types are not redefined, and
  - Default values are of the correct type.

The JSON parser will not parse schemas with invalid names or values of the
wrong type.

If working with multiple Codecs, it can be a good idea to include a test in
your test suite applying this function to your Codec schemas.

-}
validateSchema : Schema -> Result SchemaInvalid ()
validateSchema =
    let
        go allowUnionsHere schema =
            case schema of
                Union { options } ->
                    if allowUnionsHere then
                        let
                            sortedNames =
                                options
                                    |> List.map typeName
                                    |> List.sortBy (.baseName << canonicalName)

                            firstDuplicate =
                                findDuplicate (.baseName << canonicalName) sortedNames
                        in
                        case firstDuplicate of
                            Nothing ->
                                Result.traverse (go False) options
                                    |> Result.map (always ())

                            Just tn ->
                                Err (SchemaIdenticalNamesInUnion tn)

                    else
                        Err SchemaNestedUnion

                Array { items } ->
                    validateSchema items

                Map { values } ->
                    validateSchema values

                Record info ->
                    Result.traverse (goField info.name) info.fields
                        |> Result.andThen (always (validNames info))

                Enum info ->
                    let
                        duplicateCheck =
                            case findDuplicate identity (List.sort info.symbols) of
                                Just x ->
                                    Err (SchemaHasDuplicateEnumValue info.name x)

                                Nothing ->
                                    Ok ()

                        defaultCheck =
                            case info.default of
                                Just x ->
                                    if List.member x info.symbols then
                                        Ok ()

                                    else
                                        Err (SchemaEnumDefaultNotFound info.name x)

                                Nothing ->
                                    Ok ()
                    in
                    Result.map3 (\() () () -> ())
                        (validNames info)
                        duplicateCheck
                        defaultCheck

                Fixed info ->
                    validNames info

                NamedType info ->
                    Name.validName info
                        |> Result.mapError (SchemaHasInvalidName info)
                        |> Result.map (always ())

                _ ->
                    Ok ()

        validNames inf =
            case findErr Name.validName (inf.name :: inf.aliases) of
                Just ( nm, err ) ->
                    Err (SchemaHasInvalidName nm err)

                Nothing ->
                    Ok ()

        goField nm fld =
            Result.map2 (\_ _ -> ())
                (Name.validName (TypeName fld.name [])
                    |> Result.mapError (SchemaHasInvalidFieldName nm fld.name)
                )
                (validateSchema fld.type_)

        findDuplicate f xs =
            case xs of
                x :: y :: zs ->
                    if f x == f y then
                        Just x

                    else
                        findDuplicate f (y :: zs)

                _ ->
                    Nothing

        findErr : (a -> Result e b) -> List a -> Maybe ( a, e )
        findErr f =
            let
                step input =
                    case input of
                        x :: xs ->
                            case f x of
                                Err b ->
                                    Just ( x, b )

                                Ok _ ->
                                    step xs

                        _ ->
                            Nothing
            in
            step
    in
    go True


{-| Turn an Avro schema into its [canonical form](https://avro.apache.org/docs/1.11.1/specification/#parsing-canonical-form-for-schemas).

Schema canonical form can be used to determine if two schemas are
functional equivalent when writing Avro values, as it strips
documentation and aliases as well as normalises names and
rendering.

The canonical form technically refers to a formatted JSON string
with no whitespace between terms.

This function only transforms a Schema value, and one should compose
the following functions if the canonical form string is required.

    import Avro
    import Avro.Schema as Schema exposing (Schema)
    import Json.Encode as Encode

    renderCanonical : Schema -> String
    renderCanonical s =
        Schema.canonical s
            |> Avro.schemaEncoder
            |> Encode.encode 0

-}
canonicalise : Schema -> Schema
canonicalise schema =
    case schema of
        Null ->
            Null

        Boolean ->
            Boolean

        Int _ ->
            Int { logicalType = Nothing }

        Long _ ->
            Long { logicalType = Nothing }

        Float ->
            Float

        Double ->
            Double

        Bytes _ ->
            Bytes { logicalType = Nothing }

        String _ ->
            String { logicalType = Nothing }

        Array { items } ->
            Array { items = canonicalise items }

        Map { values } ->
            Map { values = canonicalise values }

        Record { name, fields } ->
            let
                canonicalField fld =
                    { name = fld.name
                    , aliases = []
                    , doc = Nothing
                    , default = Nothing
                    , order = Nothing
                    , type_ = fld.type_
                    }
            in
            Record
                { name = canonicalName name
                , aliases = []
                , doc = Nothing
                , fields = List.map canonicalField fields
                }

        Enum { name, symbols, default } ->
            Enum
                { name = canonicalName name
                , aliases = []
                , doc = Nothing
                , symbols = symbols
                , default = default
                }

        Union { options } ->
            Union { options = List.map canonicalise options }

        Fixed { name, size } ->
            Fixed
                { name = canonicalName name
                , aliases = []
                , size = size
                , logicalType = Nothing
                }

        NamedType nm ->
            NamedType (canonicalName nm)
