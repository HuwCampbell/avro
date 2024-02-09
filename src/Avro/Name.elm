module Avro.Name exposing
    ( TypeName
    , parseTypeName, contextualTypeName, canonicalName
    )

{-| Got some names


# Definition

@docs TypeName


# Definition

@docs parseTypeName, contextualTypeName, canonicalName

-}

import String


{-| An Avro Type Name
-}
type alias TypeName =
    { baseName : String
    , nameSpace : List String
    }


{-| Normalise the name
-}
canonicalName : TypeName -> TypeName
canonicalName { baseName, nameSpace } =
    let
        built =
            List.foldr (\ns rest -> ns ++ "." ++ rest) baseName nameSpace
    in
    { baseName = built, nameSpace = [] }


{-| Build a TypeName from a string
-}
parseTypeName : String -> Maybe TypeName
parseTypeName input =
    let
        splitNames =
            String.split "." input
                |> List.filter (String.isEmpty >> not)
    in
    unsnoc splitNames
        |> Maybe.map
            (\( rest, base ) ->
                TypeName base rest
            )


{-| Build a TypeName from a string
-}
contextualTypeName : String -> Maybe String -> Maybe TypeName
contextualTypeName input explicit =
    if List.isEmpty (String.indexes "." input) then
        Just (TypeName input (String.split "." (Maybe.withDefault "" explicit) |> List.filter (String.isEmpty >> not)))

    else
        parseTypeName input


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
