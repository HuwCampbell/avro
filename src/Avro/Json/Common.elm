module Avro.Json.Common exposing (optionalField, strictOptionalField)

import Json.Decode as Decode exposing (Decoder)



-- This hack is from the decoder pipeline package.
-- The idea is that you parse a value, then reparse it.
-- If there's a failure it's missing, but you can give
-- a good error message if it fails because the field is
-- wrong.


{-| Decode an optional field using the decoder, treating
null in the same way as a missing value.
-}
optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField field decoder =
    let
        nullOr =
            Decode.oneOf [ decoder |> Decode.map Just, Decode.null Nothing ]

        handleResult input =
            case Decode.decodeValue (Decode.at [ field ] Decode.value) input of
                Ok rawValue ->
                    case Decode.decodeValue nullOr rawValue of
                        Ok finalResult ->
                            Decode.succeed finalResult

                        Err _ ->
                            Decode.at [ field ] nullOr

                Err _ ->
                    Decode.succeed Nothing
    in
    Decode.value
        |> Decode.andThen handleResult


{-| Decode an optional field using the decoder, but don't treat null
in a special way.
-}
strictOptionalField : String -> Decoder a -> Decoder (Maybe a)
strictOptionalField field decoder =
    let
        mapped =
            Decode.map Just decoder

        handleResult input =
            case Decode.decodeValue (Decode.at [ field ] Decode.value) input of
                Ok rawValue ->
                    case Decode.decodeValue mapped rawValue of
                        Ok finalResult ->
                            Decode.succeed finalResult

                        Err _ ->
                            Decode.at [ field ] mapped

                Err _ ->
                    Decode.succeed Nothing
    in
    Decode.value
        |> Decode.andThen handleResult
