module Avro.Codec exposing (..)

import Avro.Internal.DList as DList exposing (DList)
import Avro.Name exposing (TypeName)
import Avro.Schema as Schema exposing (Field, Schema)
import Avro.Value as Value exposing (Value)
import Dict exposing (Dict)


type alias Codec a =
    { schema : Schema
    , decoder : Value -> Maybe a
    , writer : a -> Value
    }


{-| Invariant mapping of struct codec.
-}
imap : (b -> a) -> (a -> b) -> Codec a -> Codec b
imap g f codec =
    { schema = codec.schema
    , decoder = \values -> codec.decoder values |> Maybe.map f
    , writer = codec.writer << g
    }


{-| Definition of a List Codec

Usually one will work with this type alias
directly, as the builder is most useful when
contructing the final value.

-}
type alias StructCodec a =
    StructBuilder a a


{-| Builder for a ListCodec.

This is a profunctor which wraps a parser and a builder
for lists (this uses a DList for efficiency).

The usual pattern is to use the `using` function to chain
together a sequence of point parsers.

    success Person
        |> using string .name
        |> using int .age

-}
type alias StructBuilder b a =
    { schemas : DList Field
    , decoder : Dict String Value -> Maybe a
    , writer : b -> DList (String, Value)
    }


{-| Profunctor mapping of struct codec.
-}
dimap : (x -> y) -> (a -> b) -> StructBuilder y a -> StructBuilder x b
dimap g f codec =
    { schemas = codec.schemas
    , decoder = \values -> codec.decoder values |> Maybe.map f
    , writer = codec.writer << g
    }


{-| Mapping of codec.
-}
map : (a -> b) -> StructBuilder x a -> StructBuilder x b
map =
    dimap identity


{-| Contravariant mapping of codec.
-}
lmap : (x -> y) -> StructBuilder y a -> StructBuilder x a
lmap f =
    dimap f identity


success : a -> StructBuilder b a
success a =
    StructBuilder DList.empty (always (Just a)) (always DList.empty)


{-| Compose multiple parsers in a sequence.

This function is designed to be used with the sequencing operator (|>) and
therefore consumes its second argument first. For example:

    success Person
        |> using string .name
        |> using int .age

Will consume a string column, then an int column, and apply the constructor
Person to the arguments in the order written (string, then int).

-}
using : String -> Codec a -> (c -> a) -> StructBuilder c (a -> b) -> StructBuilder c b
using fieldName parseArg argExtract parseFunc =
    let
        schemas =
            DList.snoc
                parseFunc.schemas
                (Field fieldName [] Nothing Nothing parseArg.schema Nothing)

        decoder values =
            Maybe.map2 (\f a -> f a)
                (parseFunc.decoder values)
                (Dict.get fieldName values |> Maybe.andThen parseArg.decoder)

        writer c =
            DList.snoc
                (parseFunc.writer c)
                (fieldName, parseArg.writer (argExtract c))
    in
    StructBuilder schemas decoder writer


{-| Compose multiple parsers in a sequence.

This function is designed to be used with the sequencing operator (|>) and
therefore consumes its second argument first. For example:

    success Person
        |> using string .name
        |> using int .age

Will consume a string column, then an int column, and apply the constructor
Person to the arguments in the order written (string, then int).

-}
optional : String -> Codec a -> (c -> Maybe a) -> StructBuilder c (Maybe a -> b) -> StructBuilder c b
optional fieldName parseArg argExtract parseFunc =
    let
        optCodec =
            maybe parseArg

        schemas =
            DList.snoc
                parseFunc.schemas
                (Field fieldName [] Nothing Nothing optCodec.schema (Just Value.Null))

        decoder values =
            Maybe.map2 (\f a -> f a)
                (parseFunc.decoder values)
                (case Dict.get fieldName values of
                    Just found ->
                        optCodec.decoder found

                    Nothing ->
                        Just Nothing
                )

        writer c =
            DList.snoc
                (parseFunc.writer c)
                (fieldName, optCodec.writer (argExtract c))
    in
    StructBuilder schemas decoder writer


record : TypeName -> StructCodec a -> Codec a
record name codec =
    let
        schema =
            Schema.Record
                { name = name
                , aliases = []
                , doc = Nothing
                , fields = codec.schemas []
                }

        decoder v =
            case v of
                Value.Record _ rs ->
                    codec.decoder (Dict.fromList rs)

                _ ->
                    Nothing

        writer v =
            Value.Record name (codec.writer v [])
    in
    Codec schema decoder writer


either : Codec a -> Codec b -> Codec (Result a b)
either left right =
    let
        schema =
            Schema.Union
                { options = [ left.schema, right.schema ]
                }

        decoder v =
            case v of
                Value.Union 0 inner ->
                    left.decoder inner |> Maybe.map Err

                Value.Union 1 inner ->
                    right.decoder inner |> Maybe.map Ok

                _ ->
                    Nothing

        writer v =
            case v of
                Err l ->
                    Value.Union 0 (left.writer l)

                Ok r ->
                    Value.Union 1 (right.writer r)
    in
    Codec schema decoder writer


maybe : Codec b -> Codec (Maybe b)
maybe just =
    let
        schema =
            Schema.Union
                { options = [ Schema.Null, just.schema ]
                }

        decoder v =
            case v of
                Value.Union 0 Value.Null ->
                    Just Nothing

                Value.Union 1 inner ->
                    just.decoder inner |> Maybe.map Just

                _ ->
                    Nothing

        writer v =
            case v of
                Nothing ->
                    Value.Union 0 Value.Null

                Just r ->
                    Value.Union 1 (just.writer r)
    in
    Codec schema decoder writer


null : Codec ()
null =
    let
        parse v =
            case v of
                Value.Null ->
                    Just ()

                _ ->
                    Nothing
    in
    Codec Schema.Null parse (always Value.Null)


int : Codec Int
int =
    let
        parse v =
            case v of
                Value.Int i ->
                    Just i

                _ ->
                    Nothing
    in
    Codec Schema.Int parse Value.Int


long : Codec Int
long =
    let
        parse v =
            case v of
                Value.Long i ->
                    Just i

                _ ->
                    Nothing
    in
    Codec Schema.Long parse Value.Long


float32 : Codec Float
float32 =
    let
        parse v =
            case v of
                Value.Float i ->
                    Just i

                _ ->
                    Nothing
    in
    Codec Schema.Float parse Value.Float


float64 : Codec Float
float64 =
    let
        parse v =
            case v of
                Value.Double i ->
                    Just i

                _ ->
                    Nothing
    in
    Codec Schema.Double parse Value.Double


string : Codec String
string =
    let
        parse v =
            case v of
                Value.String i ->
                    Just i

                _ ->
                    Nothing
    in
    Codec Schema.String parse Value.String


orElse : Maybe a -> Maybe a -> Maybe a
orElse a b =
    case a of
        Just aa ->
            Just aa

        Nothing ->
            b
