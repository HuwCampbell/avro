module Avro.Name exposing
    ( TypeName
    , simpleName, parseTypeName
    , contextualTypeName
    )

{-| Got some names


# Definition

@docs TypeName


# Definition

@docs simpleName, parseTypeName

-}

import String


{-| An Avro Name

    fromList [ 'e', 'l', 'm' ] == "elm"

-}
type alias TypeName =
    { baseName : String
    , nameSpace : List String
    }


{-| Build a TypeName from a string
-}
simpleName : String -> TypeName
simpleName s =
    TypeName s []


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
