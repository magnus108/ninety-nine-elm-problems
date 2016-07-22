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

goldbach1 : Int -> Maybe (Int, Int)
goldbach1 a =
    List.head <|
        List.filter(\(x, y) -> isPrime x && isPrime y) <|
            List.map (\e -> (e, a - e)) (primeR 1 (a//2))

goldbach2 : Int -> Maybe (Int, Int)
goldbach2 a =
    let
        aux d =
            if isPrime d && isPrime (a - d) then
                Just (d, a - d)
            else if d > (a//2) then
                Nothing
            else
                aux (d + 1)
    in
        aux 2

primeR : Int -> Int -> List Int
primeR a b =
    List.filter isPrime [ a .. b]

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

solutions : List (Int -> Maybe ( Int, Int))
solutions =
    [ goldbach1
    , goldbach2
    ]

tests : List ( Int, Maybe (Int, Int), Maybe (Int, Int) ->
        Maybe (Int, Int) -> Bool )
tests =
    [ (28, Just (5,23), (==))
    , (7, Just (2, 5), (==))
    ]

test : ( Int, Maybe (Int, Int), Maybe (Int, Int) ->
        Maybe (Int, Int) -> Bool ) -> ( Int -> Maybe (Int, Int)) -> Bool
test ( result, expect, compare ) solution =
    compare (solution result) expect
