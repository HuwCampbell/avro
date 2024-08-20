module Avro.Internal.Overlay exposing (overlays)


{-| This module contains worker function for environments.

It is not currently exposed, as once items are inlines the
Schema may be invalid, as it could contain multiple versions
of a reference.

The sort of redefinition is not technically allowed.

@docs overlays

-}
import Avro.Name as Name
import Dict exposing (Dict)
import Avro.Schema exposing (Schema(..))




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
                            x

                        Nothing ->
                            s

                Record info ->
                    let
                        myNames =
                            info.name
                                :: info.aliases
                                |> List.map (Name.canonicalName >> .baseName)

                        newEnv =
                            List.foldl Dict.remove env myNames

                        goField fld =
                            { fld | type_ = go newEnv fld.type_ }

                        newFields =
                            List.map goField info.fields
                    in
                    Record { info | fields = newFields }

                Union { options } ->
                    let
                        newOptions =
                            List.map (go env) options
                    in
                    Union { options = newOptions }

                Array { items } ->
                    Array { items = go env items }

                Map { values } ->
                    Map { values = go env values }

                basic ->
                    basic
    in
    if Dict.isEmpty bindings then
        input

    else
        go bindings input
