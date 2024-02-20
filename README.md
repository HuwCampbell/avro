# Avro

Apache Avro support for Elm.

This library is the centre of Avro support, with planned libraries
for remote procedure call, code generation, and interfacing with
the Confluent Schema Registry depending on this one.

This library defines [`Codecs`](Avro-Codec#Codec), which define
Avro Schemas, as well as how to encode and decode them from Elm
data types.

As a simple example, below we describe a type alias we want to
read and write as an Avro record, then build a binary encoder for
it.

```elm
import Avro
import Avro.Codec exposing (..)
import Avro.Schema exposing (Schema, SchemaMismatch)
import Bytes.Decode exposing (Decoder)
import Bytes.Encode exposing (Encoder)

{-| Declaration of the data type we want to encode and decode.
-}
type alias Person =
    { name : String, age : Maybe Int }

{-| Build the Codec for it.

This type includes a Schema, encoder, and decoder.
-}
personCodec : Codec Person
personCodec =
    success Person
        |> requiring "name" string .name
        |> optional "age" int .age
        |> record { baseName = "person", nameSpace = ["demo"] }

{-| A byte encoder for a person.
-}
encodePerson : Person -> Encoder
encodePerson =
    Avro.makeEncoder personCodec


{-| Build a decoder for data written using a schema.
-}
decodePerson : Schema -> Result SchemaMismatch (Decoder Person)
decodePerson writerSchema =
    Avro.makeDecoder personCodec writerSchema
```

