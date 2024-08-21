module Avro.Internal.ResultExtra exposing (traverse, traverseAccumL)


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


traverseAccumL : (s -> a -> Result e ( s, b )) -> s -> List a -> Result e ( s, List b )
traverseAccumL f state list =
    traverseAccumLHelp f state list []


traverseAccumLHelp : (s -> a -> Result e ( s, b )) -> s -> List a -> List b -> Result e ( s, List b )
traverseAccumLHelp f state list acc =
    case list of
        head :: tail ->
            f state head
                |> Result.andThen
                    (\( s, a ) -> traverseAccumLHelp f s tail (a :: acc))

        [] ->
            Ok ( state, List.reverse acc )
