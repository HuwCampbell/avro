module Avro.Name exposing
    ( TypeName
    , contextualTypeName, canonicalName
    , compatibleNames, validName
    )

{-| Definitions and helpers for Avro Names.

Record, Enum and Fixed types are _named_ types. Each has a full name that is composed of two parts;
a name and a namespace. Equality of names is defined on the full name.

A namespace is list of scoping names, encoded in the interface description language and Json specification
language as a dot separated string, but here as an Elm list.

The empty string may also be used as a namespace to indicate the null namespace.
Equality of names (including field names and enum symbols) as well as fullnames is case-sensitive.

Record fields and enum symbols have names as well (but no namespace). Equality of fields and
enum symbols is defined on the name of the field/symbol within its scope (the record/enum that
defines it). Fields and enum symbols across scopes are never equal.

The name portion of the full name of named types, record field names, and enum symbols must:

  - start with [A-Za-z\_][A-Za-z_]
  - subsequently contain only [A-Za-z0-9\_][A-Za-z0-9_]


# Definition

@docs TypeName


# Functions

@docs contextualTypeName, canonicalName

@docs compatibleNames, validName

-}

import Avro.Internal.ResultExtra exposing (traverse)
import String


{-| An Avro Type Name

This constructor is exposed, but one should only build names which
are correctly scoped and have valid names.

If unsure, one should use [`contextualTypeName`](Avro-Name#contextualTypeName)
to parse the data instead.

-}
type alias TypeName =
    { baseName : String
    , nameSpace : List String
    }


{-| Normalise a TypeName.

This replaces short names with fullnames, using applicable namespaces
to do so, then eliminate namespace attributes, which are now redundant.

-}
canonicalName : TypeName -> TypeName
canonicalName { baseName, nameSpace } =
    let
        built =
            List.foldr (\ns rest -> ns ++ "." ++ rest) baseName nameSpace
    in
    { baseName = built, nameSpace = [] }


{-| Build a TypeName from a qualified string.
-}
parseFullName : String -> Result String TypeName
parseFullName input =
    case unsnoc (splitNameParts input) of
        Just ( rest, base ) ->
            Result.map2 TypeName
                (validNamePart base)
                (traverse validNamePart rest)

        Nothing ->
            Err "Type names must contain non-empty valid name parts."


{-| Build a TypeName from using the name and namespace fields within a context.

Rules for this are specified in [the avro specification](https://avro.apache.org/docs/1.11.1/specification/#names).

Arguments:

  - Optional context (parent name)
  - Name
  - Optional Namespace

-}
contextualTypeName : Maybe TypeName -> String -> Maybe String -> Result String TypeName
contextualTypeName context input explicit =
    if List.isEmpty (String.indexes "." input) then
        case explicit of
            Just ns ->
                Result.map2 TypeName
                    (validNamePart input)
                    (traverse validNamePart (splitNameParts ns))

            Nothing ->
                Result.map2 TypeName
                    (validNamePart input)
                    (Ok <| Maybe.withDefault [] <| Maybe.map .nameSpace context)

    else
        parseFullName input


{-| Test that a `TypeName` is valid

That is, test that

  - start with [A-Za-z\_][A-Za-z_]
  - subsequently contain only [A-Za-z0-9\_][A-Za-z0-9_]

-}
validName : TypeName -> Result String TypeName
validName input =
    case findErr validNamePart (input.baseName :: input.nameSpace) of
        Just x ->
            Err x

        _ ->
            Ok input


unsnoc : List b -> Maybe ( List b, b )
unsnoc list =
    let
        step x z =
            Just <|
                case z of
                    Nothing ->
                        ( [], x )

                    Just ( a, b ) ->
                        ( x :: a, b )
    in
    List.foldr step Nothing list


{-| Split a name (or namespace)

Unfortunately, the Avro specification itself has issues
around names.

The filtering of empty lists is used to fixup some of
these.

-}
splitNameParts : String -> List String
splitNameParts input =
    if String.isEmpty input then
        []

    else
        String.split "." input
            |> List.filter (String.isEmpty >> not)


validNamePart : String -> Result String String
validNamePart s =
    case String.toList s of
        c :: cs ->
            if Char.isAlpha c && List.all Char.isAlphaNum cs then
                Ok s

            else
                Err "Type name is not alpha-numeric"

        _ ->
            Err "Type name is empty"


{-| Whether two names are compatible for Schema resolution.

This means that either the unqualified names match, or an alias matches
the fully qualified name.

-}
compatibleNames : { reader | name : TypeName, aliases : List TypeName } -> { writer | name : TypeName } -> Bool
compatibleNames reader writer =
    reader.name.baseName
        == writer.name.baseName
        || List.member writer.name reader.aliases


findErr : (a -> Result e b) -> List a -> Maybe e
findErr f =
    let
        go input =
            case input of
                x :: xs ->
                    case f x of
                        Err b ->
                            Just b

                        Ok _ ->
                            go xs

                _ ->
                    Nothing
    in
    go
