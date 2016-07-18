import Html exposing (text)
import Random exposing (int)

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

gcd1 : Int -> Int -> Int
gcd1 a b =
    if b == 0 then
        abs a
    else
        gcd1 b (a % b)

solutions : List ( ( Int, Int) -> Int)
solutions =
    [ uncurry gcd1
    ]

tests : List ( (Int, Int), Int )
tests =
    [ ( (36, 63), 9 )
    , ( (-2, 2), 2 )
    ]

test : ( (Int, Int), Int ) ->
    ( (Int, Int) -> Int) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
