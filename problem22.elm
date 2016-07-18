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

range1 : Int -> Int -> List Int
range1 x y = [x..y]

solutions : List ( ( Int, Int ) -> List Int)
solutions =
    [ uncurry range1
    ]

tests : List ( (Int, Int), List Int )
tests =
    [ ( (1, 5), [1, 2, 3, 4, 5] )
    , ( (2, 3), [2, 3] )
    , ( (1, 0), [] )
    ]

test : ( ( Int, Int ), List Int ) -> (( Int, Int ) -> List Int) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
