module Avro.Codec exposing
    ( Codec
    , imap, emap
    , withDocumentation, withAliases, withLogicalType
    , int, bool, long, int64, float32, float64, null, string, bytes, array, dict, enum, namedType
    , StructCodec, StructBuilder
    , record, success, requiring, optional, withFallback, withField
    , maybe, union, union3, union4, union5
    , recursiveRecord
    , structField, using
    , dimap, lmap, map, map2, map3, map4
    )

{-| This modules defines how to build Avro Codecs.

Codec's represent how to encode and decode data for
your domain types, as well as the schema with which
they will be written.

For example, below we define a type alias and how it
would be represented as an Avro record

    type alias Person =
        { name : String, age : Maybe Int }

    personCodec : Codec Person
    personCodec =
        success Person
            |> requiring "name" string .name
            |> optional "age" int .age
            |> record { baseName = "person", nameSpace = [] }

Furthermore, one of several variants in an Avro union can represent
an Elm custom type, by using the [`union`](Avro-Codec#union) family
of functions

    personOrPetCodec : Codec (Result Person Pet)
    personOrPetCodec =
        union personCodec petCodec

Records, unions, [named types](Avro-Codec#namedType), and basic
types can be composed to easily represent complex models.


# Core Type

@docs Codec

@docs imap, emap

@docs withDocumentation, withAliases, withLogicalType


## Basic Builders

@docs int, bool, long, int64, float32, float64, null, string, bytes, array, dict, enum, namedType


# Working with Record Types

Records are easily constructed using a pipeline style to compose
field Codecs.

    success Person
        |> requiring "name" string .name
        |> optional "age" int .age
        |> record { baseName = "person", nameSpace = [] }


## Core types

@docs StructCodec, StructBuilder


## Construction

@docs record, success, requiring, optional, withFallback, withField


# Working with Union Types

Union types are similar to User defined types, and can be used to
encode them from Elm.

These functions will ensure that unions don't directly contain
other unions (which would violate the Avro specification), but
one should also ensure that type names aren't replicated in the
resulting union.

The best way to achieve this is to use these functions with record
types of distinct names where possible.

@docs maybe, union, union3, union4, union5


# Working with Recursive Types

@docs recursiveRecord


# Fancy Records

These functions are lower level than those above, but provide an
alternative method for building records.

@docs structField, using

@docs dimap, lmap, map, map2, map3, map4

-}

import Avro.Internal.DList as DList exposing (DList)
import Avro.Name exposing (TypeName)
import Avro.Schema as Schema exposing (Field, Schema, SortOrder)
import Avro.Value as Value exposing (Value)
import Avro.Value.Int64 as Int64 exposing (Int64)
import Bytes exposing (Bytes)
import Dict exposing (Dict)


{-| This type defines the schema, encoder, and decoder for an
elm type.
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
invariant functor, you can map between isomorphic types.

The main utility of this is transforming basic types to custom
types.

For example, here's the definition of the `maybe` function, which
uses `imap` with `union`.

    maybe : Codec b -> Codec (Maybe b)
    maybe just =
        let
            note =
                Result.fromMaybe ()
        in
        imap note Result.toMaybe <|
            union null just

-}
imap : (b -> a) -> (a -> b) -> Codec a -> Codec b
imap g f codec =
    { schema = codec.schema
    , decoder = codec.decoder >> Maybe.map f
    , writer = codec.writer << g
    }


{-| Partially map a Codec to a new type.

Like [`imap`](Avro-Codec#imap) this function requires a _from_ and _to_
mapping to the new type, but this also allows one to fail to parse an
unexpected value.

For example, the Logical UUID Type is backed by an Avro string.
As not all strings are valid UUIDs, this function would be used to
print and parse the string. Using the
[`elm-uuid`](https://package.elm-lang.org/packages/NoRedInk/elm-uuid/2.0.0/)
library:

    import Prng.Uuid as Uuid exposing (Uuid)

    uuid : Codec Uuid
    uuid =
        string
            |> emap Uuid.toString Uuid.fromString
            |> withLogicalType "uuid"

This function can be overused as if the mapping fails the user
will receive a parse error from the binary decoder, and lose any
specific information as to the nature of the error.

-}
emap : (b -> a) -> (a -> Maybe b) -> Codec a -> Codec b
emap g f codec =
    { schema = codec.schema
    , decoder = codec.decoder >> Maybe.andThen f
    , writer = codec.writer << g
    }


{-| Add documentation to a Codec.

If the Schema does not support documentation (i.e, it's not a Record or Enum)
this function has no effect.

-}
withDocumentation : String -> Codec a -> Codec a
withDocumentation docs codec =
    { codec | schema = Schema.withDocumentation docs codec.schema }


{-| Add aliases to a Codec.

If the Schema does not support aliases (i.e, it's not a named type)
this function has no effect.

-}
withAliases : List TypeName -> Codec a -> Codec a
withAliases docs codec =
    { codec | schema = Schema.withAliases docs codec.schema }


{-| Add a logical type annotation to a Codec.
-}
withLogicalType : String -> Codec a -> Codec a
withLogicalType logicalType codec =
    { codec | schema = Schema.withLogicalType logicalType codec.schema }


{-| Definition of a Struct Codec

Usually one will work with this type alias
directly, as the builder is most useful when
contructing the final value.

-}
type alias StructCodec a =
    StructBuilder a a


{-| Builder for a Struct Codec.

The first type parameter `b` is the type we're mapping from when writing
an Avro record, the second type parameter `a` is the type we're reading
avro data into. When a builder is completely constructed, the types `a`
and `b` will be for the same type, and can be turned into a `Codec a`
using the `record` function.

The technical term for a type like this is that `StructBuilder` is an
applicative profunctor; it wraps a parser and a builder for lists of
Avro fields (along with their Schemas).

The usual pattern is to use the [`requiring`](Avro-Codec#requiring),
or [`optional`](Avro-Codec#optional) functions to chain together a
sequence of codecs for each field in turn.

-}
type StructBuilder b a
    = StructBuilder
        { schemas : DList Field
        , decoder : List Value -> Maybe ( List Value, a )
        , writer : b -> DList Value
        }


{-| A struct builder which parses and writes no fields and always succeeds.
-}
success : a -> StructBuilder b a
success a =
    StructBuilder
        { schemas = DList.empty
        , decoder = \fs -> Just ( fs, a )
        , writer = always DList.empty
        }


{-| Compose a required field's Codecs to build a record.

This function is designed to be used in a pipeline, and therefore consumes
fields in order when written in this style. For example:

    success Person
        |> requiring "name" string .name
        |> requiring "age" int .age

Will expect a string column then an int column in the record; and apply the
constructor Person to the arguments in that order.

The explicit arguments one should write are:

  - The field name which will be written into the Avro Schema;
  - The Codec for the individual field; and
  - How to extract the fields from the type in order to write it.

The final argument in the type signature is the pipelined builder for the
record under construction.

-}
requiring : String -> Codec a -> (c -> a) -> StructBuilder c (a -> b) -> StructBuilder c b
requiring fieldName parseArg argExtract parseFunc =
    withField fieldName [] Nothing Nothing parseArg Nothing argExtract parseFunc


{-| Compose an optional field's Codec to build a record.

This will create a union in the Schema with null as the first field
and set a default value of null.

-}
optional : String -> Codec a -> (c -> Maybe a) -> StructBuilder c (Maybe a -> b) -> StructBuilder c b
optional fieldName parseArg argExtract parseFunc =
    let
        optCodec =
            maybe parseArg
    in
    withFallback fieldName optCodec Nothing argExtract parseFunc


{-| Use a field in a struct codec which falls back if it doesn't exist.

In the avro specification, the default value for Union values must be
a value from the first sub-schema of the union. If the default value
violates this contstraint it will not be emitted when serializing the
Schema to JSON.

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
  - extractor

-}
withField : String -> List String -> Maybe String -> Maybe SortOrder -> Codec a -> Maybe a -> (c -> a) -> StructBuilder c (a -> b) -> StructBuilder c b
withField fieldName aliases docs order parseArg defaultValue argExtract =
    using (structField fieldName aliases docs order parseArg defaultValue |> lmap argExtract)


{-| Profunctor mapping of struct codec.
-}
dimap : (x -> y) -> (a -> b) -> StructBuilder y a -> StructBuilder x b
dimap g f (StructBuilder codec) =
    StructBuilder
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


{-| Apply a function through struct codecs.
-}
map3 : (a -> b -> c -> d) -> StructBuilder y a -> StructBuilder y b -> StructBuilder y c -> StructBuilder y d
map3 f a b c =
    map2 f a b
        |> using c


{-| Apply a function through struct codecs.

To map across more fields, one can build additional layers with `using`.

    map5 f a b c d e =
        map4 f a b c d
            |> using e

-}
map4 : (a -> b -> c -> d -> e) -> StructBuilder y a -> StructBuilder y b -> StructBuilder y c -> StructBuilder y d -> StructBuilder y e
map4 f a b c d =
    map3 f a b c
        |> using d


{-| Apply a function across 2 codecs.

This can also be used with `success` if a part of a constructor is not to
be included in the Avro definition at all.

-}
using : StructBuilder c a -> StructBuilder c (a -> b) -> StructBuilder c b
using (StructBuilder parseArg) (StructBuilder parseFunc) =
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
    StructBuilder { schemas = schemas, decoder = decoder, writer = writer }


{-| Construct a struct parser from a Codec.

Usually one can use the [`requiring`](Avro-Codec#requiring), or [`optional`](Avro-Codec#optional)
faimly of functions to chain together a sequence of point parsers.

The earlier example is equivalent to:

    example =
        success Person
            |> using (structField name [] Nothing Nothing string Nothing |> lmap .name)
            |> using (structField age [] Nothing Nothing int Nothing |> lmap .age)

    example2 =
        map2 Person
            (structField "name" [] Nothing Nothing string Nothing |> lmap .name)
            (structField "age" [] Nothing Nothing int Nothing |> lmap .age)

-}
structField : String -> List String -> Maybe String -> Maybe SortOrder -> Codec a -> Maybe a -> StructCodec a
structField fieldName aliases docs order fieldCodec defaultValue =
    let
        schemas =
            Field fieldName aliases docs order fieldCodec.schema (defaultValue |> Maybe.map fieldCodec.writer)
                |> DList.singleton

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
    StructBuilder { schemas = schemas, decoder = decoder, writer = writer }


{-| Build a Codec for an Avro record from a StructCodec.

This function requires a "completed" StructCodec, which writes and reads
the same type.

-}
record : TypeName -> StructCodec a -> Codec a
record name (StructBuilder codec) =
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

        ( Schema.Union lopt, rightSchema ) ->
            let
                leftSize =
                    List.length lopt.options

                schema =
                    Schema.Union
                        { options = List.append lopt.options [ rightSchema ]
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

        ( leftSchema, Schema.Union ropt ) ->
            let
                schema =
                    Schema.Union
                        { options = List.append [ leftSchema ] ropt.options
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

        ( leftSchema, rightSchema ) ->
            let
                schema =
                    Schema.Union
                        { options = [ leftSchema, rightSchema ]
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

If using this in a record, it may be best to use the
[`optional`](Avro-Codec#optional) function instead, as that will
apply this function as well as setting a default value.

-}
maybe : Codec b -> Codec (Maybe b)
maybe just =
    let
        note =
            Result.fromMaybe ()
    in
    imap note Result.toMaybe <|
        union null just


{-| Construct a union from 3 codecs.
-}
union3 : Codec a -> Codec b -> Codec c -> Codec (Result a (Result b c))
union3 a b c =
    union a (union b c)


{-| Construct a union from 4 codecs.
-}
union4 : Codec a -> Codec b -> Codec c -> Codec d -> Codec (Result a (Result b (Result c d)))
union4 a b c d =
    union a (union3 b c d)


{-| Construct a union from 5 codecs.
-}
union5 : Codec a -> Codec b -> Codec c -> Codec d -> Codec e -> Codec (Result a (Result b (Result c (Result d e))))
union5 a b c d e =
    union a (union4 b c d e)


{-| Construct a Avro enumeration encoded with a 0 base index.

Arguments are:

  - The name of the enum;
  - A list of symbols which the enum represents; and
  - An optional default, used when the reader encounters a symbol from
    the writer that isn’t defined in the reader’s original set of symbols.

This can be used with [`emap`](Avro-Codec#emap) to map to a custom type.

-}
enum : TypeName -> List String -> Maybe String -> Codec Int
enum name symbols default =
    let
        schema =
            Schema.Enum
                { name = name
                , aliases = []
                , doc = Nothing
                , symbols = symbols
                , default = default
                }

        parse v =
            case v of
                Value.Enum ix ->
                    Just ix

                _ ->
                    Nothing

        render a =
            Value.Enum a
    in
    Codec schema parse render


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
    Codec Schema.Boolean parse Value.Boolean


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
    Codec (Schema.Int { logicalType = Nothing }) parse Value.Int


{-| A Codec for a long type.

_Nota bene:_ This parses to an elm `Int` type, which has a maximum
precision of 53 bits. Numbers larger than this will fail to parse,
yielding a decoding error.

-}
long : Codec Int
long =
    int64
        |> emap Int64.fromInt Int64.toInt


{-| A Codec for a long type.

_Nota bene:_ Elm integers cannot represent the full 64 bit range
on standard platforms. This Codec employs two Ints using their
32 bit range to encode a single 64 bit signed integer.

-}
int64 : Codec Int64
int64 =
    let
        parse v =
            case v of
                Value.Long i ->
                    Just i

                _ ->
                    Nothing
    in
    Codec (Schema.Long { logicalType = Nothing }) parse Value.Long


{-| A Codec for a float type

_Warning:_ This uses an elm `Float` type to encode from, which is
a 64 bit type underneath. Using `double` may be a better choice unless
an expected reader can only accept a 32 bit float.

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
    Codec (Schema.String { logicalType = Nothing }) parse Value.String


{-| A Codec for a string type
-}
bytes : Codec Bytes
bytes =
    let
        parse v =
            case v of
                Value.Bytes i ->
                    Just i

                _ ->
                    Nothing
    in
    Codec (Schema.Bytes { logicalType = Nothing }) parse Value.Bytes


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


{-| A Codec for a map type.
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


{-| Use a Codec as a Named Type.

The Schema for this Codec will only be its name. This can be
useful to separate definitions logically and share them.

When using this function, one will need to put the original
schema in an environment before resolving reader and writer
schemas.

-}
namedType : Codec a -> Codec a
namedType input =
    { schema = Schema.NamedType (Schema.typeName input.schema)
    , decoder = input.decoder
    , writer = input.writer
    }


{-| Build a record type which may be recursive, by providing a `Codec`
which embeds the record as a NamedType in the Schema.

    type LinkedList
        = LinkedList Int (Maybe LinkedList)

    linkedCodec : Codec LinkedList
    linkedCodec =
        let
            struct rec =
                success LinkedList
                    |> requiring "item" int (\(LinkedList a _) -> a)
                    |> optional "rest" rec (\(LinkedList _ a) -> a)
        in
        struct
            |> recursiveRecord { baseName = "LinkedList", nameSpace = [] }

-}
recursiveRecord : TypeName -> (Codec a -> StructCodec a) -> Codec a
recursiveRecord name applied =
    let
        structDec (StructBuilder d) =
            d.decoder

        structEnc (StructBuilder d) =
            d.writer

        decoder lazy =
            case lazy of
                Value.Record rs ->
                    structDec (rec ()) rs
                        |> Maybe.map (\( _, b ) -> b)

                _ ->
                    Nothing

        writer lazy =
            Value.Record (DList.toList (structEnc (rec ()) lazy))

        rec _ =
            applied
                { schema = Schema.NamedType name
                , decoder = decoder
                , writer = writer
                }
    in
    applied (rec () |> record name |> namedType)
        |> record name


mapPair : (a -> b) -> ( x, a ) -> ( x, b )
mapPair f ( x, a ) =
    ( x, f a )


traversePair : (a -> Maybe b) -> ( x, a ) -> Maybe ( x, b )
traversePair f ( x, a ) =
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
            f head
                |> Maybe.andThen
                    (\a -> traverseHelp f tail (a :: acc))

        [] ->
            Just (List.reverse acc)
