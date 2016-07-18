import Html exposing (text)

main =
    let
        result =
            List.map test tests `andMap` solutions
                |> toString
    in
        text result

andMap : List (a -> b) -> List a -> List b
andMap funcs vals =
    List.concatMap (\f -> List.map f vals) funcs

insertAt1 : Int -> a -> List a -> List a
insertAt1 n value list =
    List.take (n - 1) list ++ value :: List.drop (n - 1) list

insertAt2 : Int -> a -> List a -> List a
insertAt2 n value list =
    case (n, list) of
        (_, []) -> [value]
        (1, xs) -> value::xs
        (n, x::xs) ->
            x::insertAt2 (n-1) value xs

solutions : List (List Int -> List Int)
solutions =
    [ insertAt1 3 3
    , insertAt2 3 3
    ]

tests : List (List Int, List Int)
tests =
    [ ( [2, 2, 3, 3, 3, 4, 6], [2, 2, 3, 3, 3, 3, 4, 6] )
    , ( [1], [1, 3] )
    , ( [], [3] )
    ]

test : ( List Int, List Int) ->
        ( List Int -> List Int ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
