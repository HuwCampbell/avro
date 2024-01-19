module Avro.Internal.Parser exposing (..)

import Avro.Internal.ReadSchema as ReadSchema exposing (..)
import Avro.Name exposing (..)
import Avro.Value as Value exposing (Value)
import Bitwise
import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Dict


zag : Int -> Int
zag n =
    Bitwise.xor
        (Bitwise.shiftRightZfBy 1 n)
        (negate (Bitwise.and n 0x01))


getZigZag : Decoder Int
getZigZag =
    getVarInt
        |> Decode.map zag


getVarInt : Decoder Int
getVarInt =
    let
        step ( depth, acc ) =
            Decode.unsignedInt8
                |> Decode.map
                    (\b ->
                        let
                            top =
                                Bitwise.and b 0x80

                            rest =
                                Bitwise.and b 0x7F

                            updated =
                                if depth == 0 then
                                    rest

                                else
                                    Bitwise.shiftLeftBy (7 * depth) rest
                                        |> Bitwise.and acc
                        in
                        if top == 0 then
                            Decode.Done updated

                        else
                            Decode.Loop ( depth + 1, updated )
                    )
    in
    Decode.loop ( 0, 0 ) step


getRecord : Dict.Dict String Value -> List ReadField -> Decoder (Dict.Dict String Value)
getRecord defaults fields =
    let
        step ( todo, acc ) =
            case todo of
                f :: fs ->
                    makeDecoder f.fldType
                        |> Decode.map
                            (\v ->
                                Decode.Loop
                                    ( fs
                                    , if f.fldIgnored then
                                        acc

                                      else
                                        Dict.insert f.fldName v acc
                                    )
                            )

                _ ->
                    Decode.succeed (Decode.Done acc)
    in
    Decode.loop ( fields, defaults ) step


getString : Decoder String
getString =
    getZigZag
        |> Decode.andThen
            (\num ->
                Decode.string num
            )


getBlocks : (a -> b -> b) -> b -> (b -> x) -> Decoder a -> Decoder x
getBlocks cons empty extract element =
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
                                Decode.Done (extract acc)
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


makeDecoder : ReadSchema -> Decoder Value
makeDecoder schema =
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
                List.reverse
                (makeDecoder elem.item)
                |> Decode.map Value.Array

        ReadSchema.Map values ->
            getBlocks (\( k, v ) -> Dict.insert k v)
                Dict.empty
                identity
                (Decode.map2 (\a b -> ( a, b )) getString (makeDecoder values.values))
                |> Decode.map Value.Map

        ReadSchema.NamedType _ ->
            Decode.fail

        ReadSchema.Record info ->
            getRecord info.defaults info.fields
                |> Decode.map (Dict.toList >> Value.Record info.name)

        ReadSchema.Enum info ->
            getZigZag
                |> Decode.andThen
                    (\b ->
                        case index b info.symbols of
                            Just s ->
                                Decode.succeed (Value.Enum s)

                            Nothing ->
                                Decode.fail
                    )

        ReadSchema.Union schemas ->
            getZigZag
                |> Decode.andThen
                    (\b ->
                        case index b schemas.options of
                            Just s ->
                                makeDecoder s
                                    |> Decode.map (Value.Union b)

                            Nothing ->
                                Decode.fail
                    )

        ReadSchema.AsUnion inner ->
            makeDecoder inner
                |> Decode.map (Value.Union 0)

        ReadSchema.Fixed info ->
            Decode.bytes info.size
                |> Decode.map (Value.Fixed info.name)


index : Int -> List a -> Maybe a
index i xs =
    List.head (List.drop i xs)


zig : Int -> Int
zig n =
    Bitwise.xor
        (Bitwise.shiftLeftBy 1 n)
        (Bitwise.shiftRightBy 63 n)


putVarInt : Int -> Encoder
putVarInt =
    let
        go lower n =
            let
                top =
                    Bitwise.shiftRightZfBy 7 n

                rest =
                    Encode.unsignedInt8 (Bitwise.and n 0x7F)
            in
            if top == 0 then
                Encode.sequence (List.reverse (rest :: lower))

            else
                go (rest :: lower) top
    in
    go []


putZigZag : Int -> Encoder
putZigZag =
    zig >> putVarInt


sizedString s =
    Encode.sequence
        [ putZigZag (Encode.getStringWidth s)
        , Encode.string s
        ]


makeEncoder : Value -> Encoder
makeEncoder value =
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
            Encode.float32 Bytes.LE i

        Value.Bytes b ->
            Encode.sequence
                [ putZigZag (Bytes.width b)
                , Encode.bytes b
                ]

        Value.String s ->
            sizedString s

        Value.Array xs ->
            [ [ putZigZag (List.length xs) ]
            , List.map makeEncoder xs
            , [ putZigZag 0 ]
            ]
                |> List.concat
                |> Encode.sequence

        Value.Map xs ->
            [ [ putZigZag (Dict.size xs) ]
            , List.map (\( k, v ) -> Encode.sequence [ sizedString k, makeEncoder v ]) (Dict.toList xs)
            , [ putZigZag 0 ]
            ]
                |> List.concat
                |> Encode.sequence

        _ ->
            Encode.sequence []
