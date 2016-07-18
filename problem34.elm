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

coprime1 : Int -> Int -> Bool
coprime1 a b =
    gcd1 a b == 1

gcd1 : Int -> Int -> Int
gcd1 a b =
    if b == 0 then
        abs a
    else
        gcd1 b (a % b)

solutions : List ( ( Int, Int) -> Bool)
solutions =
    [ uncurry coprime1
    ]

tests : List ( (Int, Int), Bool )
tests =
    [ ( (23, 37), True )
    , ( (2, 12), False )
    ]

test : ( (Int, Int), Bool ) ->
    ( (Int, Int) -> Bool) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
