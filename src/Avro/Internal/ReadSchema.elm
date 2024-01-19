module Avro.Internal.ReadSchema exposing (..)

import Avro.Name exposing (TypeName)
import Avro.Value exposing (Value)
import Dict exposing (Dict)


type FieldStatus
    = AsIs ReadSchema
    | Ignored ReadSchema
    | Defaulted Value


type alias ReadField =
    { fldName : String
    , fldType : ReadSchema
    , fldIgnored : Bool
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
    | Array { item : ReadSchema }
    | Map { values : ReadSchema }
    | NamedType TypeName
    | Record
        { name : TypeName
        , fields : List ReadField
        , defaults : Dict String Value
        }
    | Enum
        { name : TypeName
        , symbols : List String
        }
    | Union { options : List ReadSchema }
    | Fixed
        { name : TypeName
        , size : Int
        }
    | AsUnion ReadSchema
