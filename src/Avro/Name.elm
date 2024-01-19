module Avro.Name exposing (..)


type alias TypeName =
    { baseName : String
    , nameSpace : List String
    }


simpleName : String -> TypeName
simpleName s =
    TypeName s []
