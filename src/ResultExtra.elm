module ResultExtra exposing (..)

import Dict exposing (Dict)


traverse : (a -> Result e b) -> List a -> Result e (List b)
traverse f list =
    traverseHelp f list []


traverseHelp : (a -> Result e b) -> List a -> List b -> Result e (List b)
traverseHelp f list acc =
    case list of
        head :: tail ->
            f head
                |> Result.andThen
                    (\a -> traverseHelp f tail (a :: acc))

        [] ->
            Ok (List.reverse acc)


traversePair : (a -> Result e b) -> ( x, a ) -> Result e ( x, b )
traversePair f ( x, a ) =
    f a |> Result.map (\b -> ( x, b ))


traverseDict : (a -> Result e b) -> Dict comparable a -> Result e (Dict comparable b)
traverseDict f d =
    traverse (traversePair f) (Dict.toList d)
        |> Result.map Dict.fromList


sequenceDict : Dict comparable (Result e b) -> Result e (Dict comparable b)
sequenceDict =
    traverseDict identity
