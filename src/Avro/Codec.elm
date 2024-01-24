module Avro.Codec exposing
    ( Codec
    , imap
    , int, bool, long, float32, float64, null, string, array, dict
    , StructCodec, StructBuilder
    , record, success, requiring, optional, withFallback, withField
    , maybe, union, union3, union4, union5
    , structField, using
    , dimap, lmap, map, map2, map3, map4
    )

{-| This modules defines how to build Avro Codecs.


# Core Type

@docs Codec

@docs imap


## Basic Builders

@docs int, bool, long, float32, float64, null, string, array, dict


# Working with Record Types


## Core types

@docs StructCodec, StructBuilder


## Construction

@docs record, success, requiring, optional, withFallback, withField


# Working with Union Types

Union types are similar to User defined types, and can be used to
encode them from Elm.

@docs maybe, union, union3, union4, union5


# Fancy Structs

@docs structField, using

@docs dimap, lmap, map, map2, map3, map4

-}

import Avro.Internal.DList as DList exposing (DList)
import Avro.Name exposing (TypeName)
import Avro.Schema as Schema exposing (Field, Schema)
import Avro.Value as Value exposing (Value)
import Dict exposing (Dict)


{-| An Avro Codec.

This type defines the Schema, encoder, and decoder for an
elm type.

This is best used for modelling a domain, and reading avro
values.

-}
type alias Codec a =
    { schema : Schema
    , decoder : Value -> Maybe a
    , writer : a -> Value
    }


{-| Map a Codec to a new type.

A Codec can transform data _from_ avro, as well as _to_ avro.
Therefore to change its type, you need to provide function to
transform back and forth between the original type and the new
type.

The categorical term for this is that Codec is an
invariant functor, you can only map between isomorphic types.

The main utility of this is transforming basic types to custom
types.

-}
imap : (b -> a) -> (a -> b) -> Codec a -> Codec b
imap g f codec =
    { schema = codec.schema
    , decoder = \values -> codec.decoder values |> Maybe.map f
    , writer = codec.writer << g
    }


{-| Definition of a Struct Codec

Usually one will work with this type alias
directly, as the builder is most useful when
contructing the final value.

-}
type alias StructCodec a =
    StructBuilder a a


{-| Builder for a Struct Codec.

This is a profunctor which wraps a parser and a builder
for lists (this uses a DList for efficiency).

The usual pattern is to use the [`requiring`](Avro-Codec#requiring), or [`optional`](Avro-Codec#optional)
functions to chain together a sequence of point parsers.

    success Person
        |> requiring "name" string .name
        |> requiring "age" int .age

-}
type alias StructBuilder b a =
    { schemas : DList Field
    , decoder : List Value -> Maybe ( List Value, a )
    , writer : b -> DList Value
    }


{-| A struct parser which parses no fields and always succeeds
-}
success : a -> StructBuilder b a
success a =
    StructBuilder DList.empty (\fs -> Just ( fs, a )) (always DList.empty)


{-| Compose multiple parsers in a sequence.

This function is designed to be used with the sequencing operator (|>) and
therefore consumes its second argument first. For example:

    success Person
        |> requiring "name" string .name
        |> requiring "age" int .age

Will consume a string column, then an int column, and apply the constructor
Person to the arguments in the order written (string, then int).

-}
requiring : String -> Codec a -> (c -> a) -> StructBuilder c (a -> b) -> StructBuilder c b
requiring fieldName parseArg argExtract parseFunc =
    withField fieldName [] Nothing Nothing parseArg Nothing argExtract parseFunc


{-| Use an optional field in a struct codec
-}
optional : String -> Codec a -> (c -> Maybe a) -> StructBuilder c (Maybe a -> b) -> StructBuilder c b
optional fieldName parseArg argExtract parseFunc =
    let
        optCodec =
            maybe parseArg
    in
    withFallback fieldName optCodec Nothing argExtract parseFunc


{-| Use a field in a struct codec which falls back if it doesn't exst
-}
withFallback : String -> Codec a -> a -> (c -> a) -> StructBuilder c (a -> b) -> StructBuilder c b
withFallback fieldName parseArg fallback argExtract parseFunc =
    withField fieldName [] Nothing Nothing parseArg (Just fallback) argExtract parseFunc


{-| Use a field in a struct codec, supplying all possible parts.

If you need to add documentation, ordering, or aliases, this function allows full customisation of the
struct field.

The arguments are:

  - name
  - aliases
  - documentation
  - ordering
  - point codec
  - default value

-}
withField : String -> List String -> Maybe String -> Maybe Order -> Codec a -> Maybe a -> (c -> a) -> StructBuilder c (a -> b) -> StructBuilder c b
withField fieldName aliases docs order parseArg defaultValue argExtract parseFunc =
    using (structField fieldName aliases docs order parseArg defaultValue |> lmap argExtract)
        parseFunc


{-| Profunctor mapping of struct codec.
-}
dimap : (x -> y) -> (a -> b) -> StructBuilder y a -> StructBuilder x b
dimap g f codec =
    { schemas = codec.schemas
    , decoder = \values -> codec.decoder values |> Maybe.map (mapPair f)
    , writer = codec.writer << g
    }


{-| Mapping of struct codec.
-}
map : (a -> b) -> StructBuilder x a -> StructBuilder x b
map =
    dimap identity


{-| Contravariant mapping of struct codec.
-}
lmap : (x -> y) -> StructBuilder y a -> StructBuilder x a
lmap f =
    dimap f identity


{-| Apply a function through struct codecs.

This can be used as an alternative to [`using`](Avro-Codec#using)

-}
map2 : (a -> b -> c) -> StructBuilder y a -> StructBuilder y b -> StructBuilder y c
map2 f a b =
    map f a
        |> using b


{-| -}
map3 : (a -> b -> c -> d) -> StructBuilder y a -> StructBuilder y b -> StructBuilder y c -> StructBuilder y d
map3 f a b c =
    map2 f a b
        |> using c


{-| -}
map4 : (a -> b -> c -> d -> e) -> StructBuilder y a -> StructBuilder y b -> StructBuilder y c -> StructBuilder y d -> StructBuilder y e
map4 f a b c d =
    map3 f a b c
        |> using d


{-| Apply a function across 2 codecs.
-}
using : StructBuilder c a -> StructBuilder c (a -> b) -> StructBuilder c b
using parseArg parseFunc =
    let
        schemas =
            DList.append
                parseFunc.schemas
                parseArg.schemas

        decoder values =
            parseFunc.decoder values
                |> Maybe.andThen
                    (\( remaining, f ) ->
                        parseArg.decoder remaining
                            |> Maybe.map (\( left, a ) -> ( left, f a ))
                    )

        writer c =
            DList.append
                (parseFunc.writer c)
                (parseArg.writer c)
    in
    StructBuilder schemas decoder writer


{-| Construct a struct parser from a Codec.

Usually one can use the [`requiring`](Avro-Codec#requiring), or [`optional`](Avro-Codec#optional)
faimly of functions to chain together a sequence of point parsers.

The earlier example is equivalent to:

    example =
        success Person
            |> using (structField name [] Nothing Nothing string Nothing |> lmap .name)
            |> using (structField age [] Nothing Nothing int Nothing |> lmap .name)

-}
structField : String -> List String -> Maybe String -> Maybe Order -> Codec a -> Maybe a -> StructCodec a
structField fieldName aliases docs order fieldCodec defaultValue =
    let
        field =
            Field fieldName aliases docs order fieldCodec.schema (defaultValue |> Maybe.map fieldCodec.writer)

        schemas =
            DList.singleton
                field

        decoder values =
            case values of
                g :: gs ->
                    fieldCodec.decoder g
                        |> Maybe.map (\a -> ( gs, a ))

                _ ->
                    Nothing

        writer c =
            DList.singleton
                (fieldCodec.writer c)
    in
    StructBuilder schemas decoder writer


{-| Built a Codec from a StructCodec
-}
record : TypeName -> StructCodec a -> Codec a
record name codec =
    let
        schema =
            Schema.Record
                { name = name
                , aliases = []
                , doc = Nothing
                , fields = DList.toList codec.schemas
                }

        decoder v =
            case v of
                Value.Record rs ->
                    codec.decoder rs
                        |> Maybe.map (\( _, b ) -> b)

                _ ->
                    Nothing

        writer v =
            Value.Record (DList.toList (codec.writer v))
    in
    Codec schema decoder writer


{-| Construct a Codec for an Avro union.

As Avro unions can not be nested (i.e., they can not directly contain
other unions) this function takes care to flatten unions passed in to
it.

Often it is useful to use the [`imap`](Avro-Codec#imap) function to
turn this into a Custom Type.

-}
union : Codec a -> Codec b -> Codec (Result a b)
union left right =
    case ( left.schema, right.schema ) of
        ( Schema.Union lopt, Schema.Union ropt ) ->
            let
                leftSize =
                    List.length lopt.options

                schema =
                    Schema.Union
                        { options = List.append lopt.options ropt.options
                        }

                decoder v =
                    case v of
                        Value.Union ix inner ->
                            if ix < leftSize then
                                left.decoder (Value.Union ix inner) |> Maybe.map Err

                            else
                                right.decoder (Value.Union (ix - leftSize) inner) |> Maybe.map Ok

                        _ ->
                            Nothing

                writer v =
                    case v of
                        Err l ->
                            left.writer l

                        Ok r ->
                            case right.writer r of
                                Value.Union ix written ->
                                    Value.Union (ix + leftSize) written

                                other ->
                                    other
            in
            Codec schema decoder writer

        ( Schema.Union lopt, _ ) ->
            let
                leftSize =
                    List.length lopt.options

                schema =
                    Schema.Union
                        { options = List.append lopt.options [ right.schema ]
                        }

                decoder v =
                    case v of
                        Value.Union ix inner ->
                            if ix < leftSize then
                                left.decoder (Value.Union ix inner) |> Maybe.map Err

                            else
                                right.decoder inner |> Maybe.map Ok

                        _ ->
                            Nothing

                writer v =
                    case v of
                        Err l ->
                            left.writer l

                        Ok r ->
                            Value.Union leftSize (right.writer r)
            in
            Codec schema decoder writer

        ( _, Schema.Union ropt ) ->
            let
                schema =
                    Schema.Union
                        { options = List.append [ right.schema ] ropt.options
                        }

                decoder v =
                    case v of
                        Value.Union ix inner ->
                            if ix == 0 then
                                left.decoder inner |> Maybe.map Err

                            else
                                right.decoder (Value.Union (ix - 1) inner) |> Maybe.map Ok

                        _ ->
                            Nothing

                writer v =
                    case v of
                        Err l ->
                            Value.Union 0 (left.writer l)

                        Ok r ->
                            case right.writer r of
                                Value.Union ix written ->
                                    Value.Union (ix + 1) written

                                other ->
                                    other
            in
            Codec schema decoder writer

        ( _, _ ) ->
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


{-| A codec for a potentially missing value.

If using this in a record, it might be best to use the
[`optional`](Avro-Codec#optional) function instead, as that will
call this an also set a null default value.

-}
maybe : Codec b -> Codec (Maybe b)
maybe just =
    let
        note m =
            case m of
                Just x ->
                    Ok x

                Nothing ->
                    Err ()

        hush m =
            case m of
                Ok x ->
                    Just x

                Err () ->
                    Nothing
    in
    imap note hush <|
        union null just


{-| Union 3

Construct a union from 3 codecs.

-}
union3 : Codec a -> Codec b -> Codec c -> Codec (Result a (Result b c))
union3 a b c =
    union a (union b c)


{-| Union 4

Construct a union from 4 codecs.

-}
union4 : Codec a -> Codec b -> Codec c -> Codec d -> Codec (Result a (Result b (Result c d)))
union4 a b c d =
    union a (union3 b c d)


{-| Union 5
-}
union5 : Codec a -> Codec b -> Codec c -> Codec d -> Codec e -> Codec (Result a (Result b (Result c (Result d e))))
union5 a b c d e =
    union a (union4 b c d e)


{-| A Codec for an avro null type
-}
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


{-| A Codec for a boolean type
-}
bool : Codec Bool
bool =
    let
        parse v =
            case v of
                Value.Boolean b ->
                    Just b

                _ ->
                    Nothing
    in
    Codec Schema.Int parse Value.Boolean


{-| A Codec for an int type
-}
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


{-| A Codec for a long type.

_Warning:_ This parses to an elm `Int` type, which has a maximum
precision of 53 bits of precision. Numbers larger than this may
lose precision.

-}
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


{-| A Codec for a float type
-}
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


{-| A Codec for a double type
-}
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


{-| A Codec for a string type
-}
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


{-| A Codec for an array type
-}
array : Codec a -> Codec (List a)
array element =
    let
        schema =
            Schema.Array
                { items = element.schema
                }

        decoder v =
            case v of
                Value.Array items ->
                    traverse element.decoder items

                _ ->
                    Nothing

        writer vs =
            List.map element.writer vs
                |> Value.Array
    in
    Codec schema decoder writer


{-| A Codec for a map type
-}
dict : Codec a -> Codec (Dict String a)
dict element =
    let
        schema =
            Schema.Map
                { values = element.schema
                }

        decoder v =
            case v of
                Value.Map items ->
                    traverseDict element.decoder items

                _ ->
                    Nothing

        writer vs =
            Dict.map (always element.writer) vs
                |> Value.Map
    in
    Codec schema decoder writer


mapPair : (a -> b) -> ( x, a ) -> ( x, b )
mapPair f p =
    case p of
        ( x, a ) ->
            ( x, f a )


traversePair : (a -> Maybe b) -> ( x, a ) -> Maybe ( x, b )
traversePair f p =
    case p of
        ( x, a ) ->
            f a |> Maybe.map (\b -> ( x, b ))


traverseDict : (a -> Maybe b) -> Dict comparable a -> Maybe (Dict comparable b)
traverseDict f d =
    traverse (traversePair f) (Dict.toList d)
        |> Maybe.map Dict.fromList


traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f list =
    traverseHelp f list []


traverseHelp : (a -> Maybe b) -> List a -> List b -> Maybe (List b)
traverseHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                Just a ->
                    traverseHelp f tail (a :: acc)

                Nothing ->
                    Nothing

        [] ->
            Just (List.reverse acc)
