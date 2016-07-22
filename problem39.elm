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

primeR1 : Int -> Int -> List Int
primeR1 a b =
    List.filter isPrime [ a .. b]

primeR2 : Int -> Int -> List Int
primeR2 a b =
    if a > b then
        []
    else
        let
            rest = primeR2 (a + 1) b
        in
            if isPrime a then
                a :: rest
            else
                rest

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

solutions : List ((Int, Int) -> List Int)
solutions =
    [ uncurry primeR1
    , uncurry primeR2
    ]

tests : List ((Int, Int), List Int, List Int -> List Int -> Bool )
tests =
    [ ( (10, 20), [11, 13, 17, 19], (==)  )
    ]

test : ( (Int, Int), List Int, List Int -> List Int -> Bool ) ->
      ((Int, Int) -> List Int) -> Bool
test ( result, expect, compare ) solution =
    compare (solution result) expect
