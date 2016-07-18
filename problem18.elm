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

slice1 : Int -> Int -> List a -> List a
slice1 j k = List.drop (j - 1) >> List.take (k - j + 1)

solutions : List (List a -> List a)
solutions =
    [ slice1 2 5
    ]

tests : List (List Int, List Int)
tests =
    [ ( [2, 2, 3, 3, 3, 4, 6], [2, 3, 3, 3] )
    , ( [1], [] )
    , ( [], [] )
    ]

test : ( List Int, List Int) ->
        ( List Int -> List Int ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
