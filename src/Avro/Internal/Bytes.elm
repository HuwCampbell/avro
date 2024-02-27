module Avro.Internal.Bytes exposing
    ( Environment(..)
    , emptyEnvironment
    , encodeValue
    , environmentNames
    , makeDecoder
    , makeDecoderEnvironment
    )

import Array
import Avro.Name exposing (..)
import Avro.ReadSchema as ReadSchema exposing (..)
import Avro.Value as Value exposing (Value)
import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.DecodeExtra as Decode
import Bytes.Encode as Encode exposing (Encoder)
import Bytes.VarInt exposing (getZigZag, putZigZag)
import Dict exposing (Dict)


getRecord : Environment -> Dict Int Value -> List ReadField -> Decoder (List Value)
getRecord env defaults fields =
    let
        step ( todo, acc ) =
            case todo of
                f :: fs ->
                    makeDecoder env f.fldType
                        |> Decode.map
                            (\v ->
                                Decode.Loop
                                    ( fs
                                    , case f.fldPosition of
                                        Nothing ->
                                            acc

                                        Just pos ->
                                            Dict.insert pos v acc
                                    )
                            )

                _ ->
                    Decode.succeed (Decode.Done (Dict.values acc))
    in
    Decode.loop ( fields, defaults ) step


getString : Decoder String
getString =
    getZigZag
        |> Decode.andThen
            (\num ->
                Decode.string num
            )


getBlocks : (a -> b -> b) -> b -> Decoder a -> Decoder b
getBlocks cons empty element =
    let
        step ( state, acc ) =
            if state > 0 then
                element
                    |> Decode.map
                        (\e ->
                            Decode.Loop ( state - 1, cons e acc )
                        )

            else
                getZigZag
                    |> Decode.andThen
                        (\b ->
                            if b == 0 then
                                Decode.Done acc
                                    |> Decode.succeed

                            else if b < 0 then
                                getZigZag
                                    |> Decode.map
                                        (\_ ->
                                            Decode.Loop ( negate b, acc )
                                        )

                            else
                                Decode.Loop ( b, acc )
                                    |> Decode.succeed
                        )
    in
    Decode.loop ( 0, empty ) step


type Environment
    = Env (Dict String (Environment -> Decoder Value))


emptyEnvironment : Environment
emptyEnvironment =
    Env Dict.empty


environmentNames : Environment -> List String
environmentNames (Env env) =
    Dict.keys env


makeDecoderEnvironment : List ( String, ReadSchema ) -> Environment
makeDecoderEnvironment namedPairs =
    namedPairs
        |> List.map (\( name, readSchema ) -> ( name, \environment -> makeDecoder environment readSchema ))
        |> Dict.fromList
        |> Env


makeDecoder : Environment -> ReadSchema -> Decoder Value
makeDecoder ((Env envDict) as env) schema =
    case schema of
        ReadSchema.Null ->
            Decode.succeed Value.Null

        ReadSchema.Boolean ->
            Decode.unsignedInt8
                |> Decode.map (\b -> Value.Boolean (b >= 1))

        ReadSchema.Int ->
            getZigZag
                |> Decode.map Value.Int

        ReadSchema.IntAsLong ->
            getZigZag
                |> Decode.map Value.Long

        ReadSchema.IntAsFloat ->
            getZigZag
                |> Decode.map (Value.Float << toFloat)

        ReadSchema.IntAsDouble ->
            getZigZag
                |> Decode.map (Value.Double << toFloat)

        ReadSchema.Long ->
            getZigZag
                |> Decode.map Value.Long

        ReadSchema.LongAsFloat ->
            getZigZag
                |> Decode.map (Value.Float << toFloat)

        ReadSchema.LongAsDouble ->
            getZigZag
                |> Decode.map (Value.Double << toFloat)

        ReadSchema.Float ->
            Decode.float32 Bytes.LE
                |> Decode.map Value.Float

        ReadSchema.FloatAsDouble ->
            Decode.float32 Bytes.LE
                |> Decode.map Value.Double

        ReadSchema.Double ->
            Decode.float64 Bytes.LE
                |> Decode.map Value.Double

        ReadSchema.Bytes ->
            getZigZag
                |> Decode.andThen (Decode.bytes >> Decode.map Value.Bytes)

        ReadSchema.String ->
            getString
                |> Decode.map Value.String

        ReadSchema.Array elem ->
            getBlocks (::)
                []
                (makeDecoder env elem.items)
                |> Decode.map (List.reverse >> Value.Array)

        ReadSchema.Map values ->
            getBlocks (\( k, v ) -> Dict.insert k v)
                Dict.empty
                (Decode.map2 (\a b -> ( a, b )) getString (makeDecoder env values.values))
                |> Decode.map Value.Map

        ReadSchema.Record info ->
            let
                runRecord envWithSelf =
                    getRecord envWithSelf info.defaults info.fields
                        |> Decode.map Value.Record

                newEnv =
                    Dict.insert (canonicalName info.name).baseName runRecord envDict
                        |> Env
            in
            runRecord newEnv

        ReadSchema.Enum info ->
            getZigZag
                |> Decode.andThen
                    (\loc ->
                        case Array.get loc info.symbols of
                            Just ix ->
                                Decode.succeed (Value.Enum ix)

                            Nothing ->
                                Decode.fail
                    )

        ReadSchema.Union schemas ->
            getZigZag
                |> Decode.andThen
                    (\b ->
                        case index b schemas.options of
                            Just ( ix, s ) ->
                                makeDecoder env s
                                    |> Decode.map (Value.Union ix)

                            Nothing ->
                                Decode.fail
                    )

        ReadSchema.AsUnion ix inner ->
            makeDecoder env inner
                |> Decode.map (Value.Union ix)

        ReadSchema.Fixed info ->
            Decode.bytes info.size
                |> Decode.map (Value.Fixed info.name)

        ReadSchema.NamedType nt ->
            case Dict.get (canonicalName nt).baseName envDict of
                Just discovered ->
                    discovered env

                Nothing ->
                    Decode.fail


index : Int -> List a -> Maybe a
index i xs =
    List.head (List.drop i xs)


sizedString : String -> Encoder
sizedString s =
    Encode.sequence
        [ putZigZag (Encode.getStringWidth s)
        , Encode.string s
        ]


encodeValue : Value -> Encoder
encodeValue value =
    case value of
        Value.Null ->
            Encode.sequence []

        Value.Boolean b ->
            if b then
                Encode.unsignedInt8 1

            else
                Encode.unsignedInt8 0

        Value.Int i ->
            putZigZag i

        Value.Long i ->
            putZigZag i

        Value.Float i ->
            Encode.float32 Bytes.LE i

        Value.Double i ->
            Encode.float64 Bytes.LE i

        Value.Bytes b ->
            Encode.sequence
                [ putZigZag (Bytes.width b)
                , Encode.bytes b
                ]

        Value.String s ->
            sizedString s

        Value.Array [] ->
            putZigZag 0

        Value.Array xs ->
            [ [ putZigZag (List.length xs) ]
            , List.map encodeValue xs
            , [ putZigZag 0 ]
            ]
                |> List.concat
                |> Encode.sequence

        Value.Map xs ->
            if Dict.isEmpty xs then
                putZigZag 0

            else
                [ [ putZigZag (Dict.size xs) ]
                , List.map (\( k, v ) -> Encode.sequence [ sizedString k, encodeValue v ]) (Dict.toList xs)
                , [ putZigZag 0 ]
                ]
                    |> List.concat
                    |> Encode.sequence

        Value.Record items ->
            List.map encodeValue items
                |> Encode.sequence

        Value.Union ix item ->
            [ putZigZag ix
            , encodeValue item
            ]
                |> Encode.sequence

        Value.Fixed _ bytes ->
            Encode.bytes bytes

        Value.Enum i ->
            putZigZag i
