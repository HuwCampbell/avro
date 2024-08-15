module Avro.Internal.ResultExtra exposing (traverse, traverse_)


traverse_ : (a -> Result e b) -> List a -> Result e ()
traverse_ f list =
    traverse f list
        |> Result.map (always ())


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
