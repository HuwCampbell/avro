module Avro.ReadSchema exposing (ReadField, ReadSchema(..))

import Avro.Name exposing (TypeName)
import Avro.Value exposing (Value)
import Dict exposing (Dict)


type alias ReadField =
    { fldName : String
    , fldType : ReadSchema
    , fldPosition : Maybe Int
    }


type ReadSchema
    = Null
    | Boolean
    | Int
    | IntAsLong
    | IntAsFloat
    | IntAsDouble
    | Long
    | LongAsFloat
    | LongAsDouble
    | Float
    | FloatAsDouble
    | Double
    | Bytes
    | String
    | Array { items : ReadSchema }
    | Map { values : ReadSchema }
    | NamedType TypeName
    | Record
        { name : TypeName
        , fields : List ReadField
        , defaults : Dict Int Value
        }
    | Enum
        { name : TypeName
        , symbols : List String
        }
    | Union { options : List ( Int, ReadSchema ) }
    | Fixed
        { name : TypeName
        , size : Int
        }
    | AsUnion Int ReadSchema
