module Avro.Internal.ReadSchema exposing (ReadField, ReadSchema(..))

import Array exposing (Array)
import Avro.Internal.Value exposing (Value)
import Avro.Name exposing (TypeName)
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
        , symbols : Array Int
        }
    | Union { options : List ( Int, ReadSchema ) }
    | Fixed
        { name : TypeName
        , size : Int
        }
    | AsUnion Int ReadSchema
