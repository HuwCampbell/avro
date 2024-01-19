module CodecSpecs exposing (..)

import Avro.Codec exposing (..)
import Avro.Internal.Parser as Internal
import Avro.Internal.ReadSchema as ReadSchema
import Avro.Name
import Avro.Schema as Schema
import Avro.Value as Value
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Expect
import Test exposing (..)


encodeBytes : List Int -> Bytes
encodeBytes bs =
    List.map (\b -> Encode.unsignedInt8 b) bs
        |> Encode.sequence
        |> Encode.encode


type alias Person =
    { who : String, age : Int, sex : Maybe String }


personCodec : Codec Person
personCodec =
    success Person
        |> using "who" string .who
        |> using "age" int .age
        |> optional "sex" string .sex
        |> record { baseName = "person", nameSpace = [] }


readSchemaOf : Schema.Schema -> Maybe ReadSchema.ReadSchema
readSchemaOf s =
    Schema.deconflict s s


suite : Test
suite =
    describe "The Codecs Module"
        []
