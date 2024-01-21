module Avro.Name exposing
    ( TypeName
    , simpleName
    )

{-| Got some names


# Definition

@docs TypeName


# Definition

@docs simpleName

-}


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
