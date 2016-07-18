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

rotate1 : Int -> List a -> List a
rotate1 n list = List.drop n list ++ List.take n list

solutions : List (List a -> List a)
solutions =
    [ rotate1 3
    ]

tests : List (List Int, List Int)
tests =
    [ ( [2, 2, 3, 3, 3, 4, 6], [3, 3, 4, 6, 2, 2, 3] )
    , ( [1], [1] )
    , ( [], [] )
    ]

test : ( List Int, List Int) ->
        ( List Int -> List Int ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
