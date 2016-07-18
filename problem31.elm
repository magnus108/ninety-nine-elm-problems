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

isPrime : Int -> Bool
isPrime n =
    if n < 2 then
        False
    else
        eratos (abs n) [2..(n // 2)]

eratos : Int -> List Int -> Bool
eratos n list =
    case list of
        [] -> True
        x::xs ->
            if n % x == 0 then
                False
            else
                eratos n (List.filter (\y -> (y % x) /= 0) xs)

solutions : List (Int -> Bool)
solutions =
    [ isPrime
    ]

tests : List ( Int, Bool )
tests =
    [ ( 7, True )
    , ( 13, True )
    , ( 1, False )
    , ( 35, False )
    ]

test : ( Int, Bool ) ->
    ( Int -> Bool) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
