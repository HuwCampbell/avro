module Avro.Internal.Overlay exposing (overlays)

{-| This module contains worker function for environments.

It is not currently exposed, as once items are inlined the
Schema may be invalid, as it could contain multiple versions
of a reference.

The sort of redefinition is not technically allowed.

@docs overlays

-}

import Avro.Name as Name exposing (TypeName)
import Avro.Schema exposing (Schema(..))
import Dict exposing (Dict)


{-| Traverse a schema and build a map of all declared types.
-}
extractBindings : Schema -> Dict String Schema
extractBindings s =
    let
        selfBindings info =
            info.name
                :: info.aliases
                |> List.map (\n -> ( Name.canonicalName n |> .baseName, s ))
                |> Dict.fromList
    in
    case s of
        Record info ->
            let
                deeper =
                    List.foldl Dict.union Dict.empty <|
                        List.map (extractBindings << .type_) info.fields
            in
            Dict.union (selfBindings info) deeper

        Enum info ->
            selfBindings info

        Fixed info ->
            selfBindings info

        Union info ->
            List.foldl Dict.union Dict.empty <|
                List.map extractBindings info.options

        Array { items } ->
            extractBindings items

        Map { values } ->
            extractBindings values

        _ ->
            Dict.empty


{-| Substitute named types into a Schema.

It is common when building Avro schemas traditionally to write individual
types separately and compose them into larger objects.

This function will rebuild a complete Schema from small components so that
it is ready to encode and decode data.

Schemas are also parsed, in a depth first manner, left to right, allowing
fields in a record to use types defined in earlier fields (for example).

-}
overlays : Schema -> List Schema -> Schema
overlays input supplements =
    let
        bindings =
            List.foldl (Dict.union << extractBindings) Dict.empty supplements

        go env s =
            case s of
                NamedType nm ->
                    case Dict.get (Name.canonicalName nm |> .baseName) env of
                        Just x ->
                            ( env, x )

                        Nothing ->
                            ( env, s )

                (Enum info) as self ->
                    ( adjust info self env, self )

                (Fixed info) as self ->
                    ( adjust info self env, self )

                (Record info) as self ->
                    let
                        --
                        -- Ensure we don't inline recursive
                        -- values. They need to stay as refs
                        -- when we deconflict.
                        myNames =
                            info.name
                                :: info.aliases
                                |> List.map (Name.canonicalName >> .baseName)

                        newEnv =
                            List.foldl Dict.remove env myNames

                        goField e fld =
                            let
                                ( ns, nt ) =
                                    go e fld.type_
                            in
                            ( ns, { fld | type_ = nt } )

                        ( fin, newFields ) =
                            mapAccumL goField newEnv info.fields
                    in
                    ( adjust info self fin, Record { info | fields = newFields } )

                Union { options } ->
                    let
                        ( fin, newOptions ) =
                            mapAccumL go env options
                    in
                    ( fin, Union { options = newOptions } )

                Array { items } ->
                    let
                        ( fin, newItems ) =
                            go env items
                    in
                    ( fin, Array { items = newItems } )

                Map { values } ->
                    let
                        ( fin, newValues ) =
                            go env values
                    in
                    ( fin, Map { values = newValues } )

                basic ->
                    ( env, basic )

        ( _, output ) =
            go bindings input
    in
    output


adjust : { a | name : TypeName, aliases : List TypeName } -> Schema -> Dict String Schema -> Dict String Schema
adjust info self env =
    let
        myNames =
            info.name
                :: info.aliases
                |> List.map (Name.canonicalName >> .baseName)

        ins n e =
            Dict.insert n self e
    in
    List.foldl ins env myNames


mapAccumL : (s -> a -> ( s, b )) -> s -> List a -> ( s, List b )
mapAccumL f state list =
    mapAccumLHelp f state list []


mapAccumLHelp : (s -> a -> ( s, b )) -> s -> List a -> List b -> ( s, List b )
mapAccumLHelp f state list acc =
    case list of
        head :: tail ->
            let
                ( s, a ) =
                    f state head
            in
            mapAccumLHelp f s tail (a :: acc)

        [] ->
            ( state, List.reverse acc )
