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

split1 : Int -> List a -> (List a, List a )
split1 n list = (List.take n list, List.drop n list)

solutions : List (List a -> (List a, List a))
solutions =
    [ split1 2
    ]

tests : List (List Int, (List Int,List Int))
tests =
    [ ( [2, 2, 3], ([2, 2],[3]) )
    , ( [1], ([1],[]) )
    , ( [], ([],[]) )
    ]

test : ( List Int, (List Int, List Int)) ->
        ( List Int -> (List Int, List Int) ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
