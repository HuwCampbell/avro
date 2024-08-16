module Resolution.Base exposing (..)

import Avro
import Avro.Codec exposing (Codec)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect


compatible : Codec a -> Codec b -> a -> b -> Expect.Expectation
compatible reader writer expect written =
    case Avro.makeDecoder reader writer.schema of
        Err schemaError ->
            Expect.ok (Err schemaError)

        Ok decoder ->
            let
                encoded =
                    Avro.makeEncoder writer written
                        |> Encode.encode

                decoded =
                    Decode.decode decoder encoded
            in
            Expect.equal decoded (Just expect)
