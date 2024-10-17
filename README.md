# Avro

[Apache Avro™ ](https://avro.apache.org/) support for Elm.

Apache Avro™ is a leading serialisation format for record data,
with a powerful type system, and great support for schema evolution.

This library offers comprehensive support for reading and writing
Avro binary data to Elm types, through the definition of
[`Codecs`](https://package.elm-lang.org/packages/HuwCampbell/avro/2.0.0/Avro-Codec/#Codec).
These describe Avro Schemas, as well as encoders and decoders for
Avro values.

As a simple example, below we define an Elm record with a type alias,
and then build a binary Codec for it.

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

