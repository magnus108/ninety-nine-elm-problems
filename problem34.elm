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

totient1 : Int -> Int
totient1 a =
    [1..a]
    |> List.filter (coprime1 a)
    |> List.length

coprime1 : Int -> Int -> Bool
coprime1 a b =
    gcd1 a b == 1

gcd1 : Int -> Int -> Int
gcd1 a b =
    if b == 0 then
        abs a
    else
        gcd1 b (a % b)

solutions : List ( Int -> Int )
solutions =
    [ totient1
    ]

tests : List ( Int, Int )
tests =
    [ ( 10, 4 )
    , ( 1, 1 )
    ]

test : ( Int, Int ) -> ( Int -> Int) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
